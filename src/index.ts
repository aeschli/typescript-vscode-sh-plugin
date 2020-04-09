
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

import { TokenType, TokenModifier, TokenEncodingConsts, VersionRequirement } from './constants';

export = function init(modules: { typescript: typeof import("typescript/lib/tsserverlibrary") }) {
	const ts = modules.typescript;

	function hasVersion(requiredMajor: number, requiredMinor: number) {
		const parts = ts.version.split('.');
		const majorVersion = Number(parts[0]);
		return majorVersion < requiredMajor || ((majorVersion === requiredMajor) && requiredMinor <= Number(parts[1]));
	}

	function decorate(languageService: ts.LanguageService, logger?: ts.server.Logger) {

		const intercept: Partial<ts.LanguageService> = Object.create(null);

		if (!hasVersion(VersionRequirement.major, VersionRequirement.minor)) {
			logger?.msg(`typescript-vscode-sh-plugin not active, version ${VersionRequirement.major}.${VersionRequirement.minor} required, is ${ts.version}`, ts.server.Msg.Info);
			return languageService;
		}
		logger?.msg(`typescript-vscode-sh-plugin initialized. Intercepting getEncodedSemanticClassifications and getEncodedSyntacticClassifications.`, ts.server.Msg.Info);

		intercept.getEncodedSemanticClassifications = (filename: string, span: ts.TextSpan) => {
			return {
				spans: getSemanticTokens(languageService, filename, span),
				endOfLineState: ts.EndOfLineState.None
			}
		};

		intercept.getEncodedSyntacticClassifications = (_filename: string, _span: ts.TextSpan) => {
			return {
				spans: [],
				endOfLineState: ts.EndOfLineState.None
			}
		};

		return new Proxy(languageService, {
			get: (target: any, property: keyof ts.LanguageService) => {
				return intercept[property] || target[property];
			},
		});
	}

	function getSemanticTokens(jsLanguageService: ts.LanguageService, fileName: string, span: ts.TextSpan): number[] {
		let resultTokens: number[] = [];

		const collector = (node: ts.Node, typeIdx: number, modifierSet: number) => {
			resultTokens.push(node.getStart(), node.getWidth(), ((typeIdx + 1) << TokenEncodingConsts.typeOffset) + modifierSet);
		};
		const program = jsLanguageService.getProgram();
		if (program) {
			const sourceFile = program.getSourceFile(fileName);
			if (sourceFile) {
				collectTokens(program, sourceFile, span, collector);
			}
		}
		return resultTokens;
	}

	function collectTokens(program: ts.Program, sourceFile: ts.SourceFile, span: ts.TextSpan, collector: (node: ts.Node, tokenType: number, tokenModifier: number) => void) {
		const typeChecker = program.getTypeChecker();

		let inJSXElement = false;

		function visit(node: ts.Node) {
			if (!node || !ts.textSpanIntersectsWith(span, node.pos, node.getFullWidth()) || node.getFullWidth() === 0) {
				return;
			}
			const prevInJSXElement = inJSXElement;
			if (ts.isJsxElement(node) || ts.isJsxSelfClosingElement(node)) {
				inJSXElement = true;
			}
			if (ts.isJsxExpression(node)) {
				inJSXElement = false;
			}

			if (ts.isIdentifier(node) && !inJSXElement && !inImportClause(node)) {
				let symbol = typeChecker.getSymbolAtLocation(node);
				if (symbol) {
					if (symbol.flags & ts.SymbolFlags.Alias) {
						symbol = typeChecker.getAliasedSymbol(symbol);
					}
					let typeIdx = classifySymbol(symbol, getMeaningFromLocation(node));
					if (typeIdx !== undefined) {
						let modifierSet = 0;
						if (node.parent) {
							const parentIsDeclaration = (ts.isBindingElement(node.parent) || tokenFromDeclarationMapping[node.parent.kind] === typeIdx);
							if (parentIsDeclaration && (<ts.NamedDeclaration>node.parent).name === node) {
								modifierSet = 1 << TokenModifier.declaration;
							}
						}

						// property declaration in constructor
						if (typeIdx === TokenType.parameter && isRightSideOfQualifiedNameOrPropertyAccess(node)) {
							typeIdx = TokenType.property;
						}

						typeIdx = reclassifyByType(typeChecker, node, typeIdx);

						const decl = symbol.valueDeclaration;
						if (decl) {
							const modifiers = ts.getCombinedModifierFlags(decl);
							const nodeFlags = ts.getCombinedNodeFlags(decl);
							if (modifiers & ts.ModifierFlags.Static) {
								modifierSet |= 1 << TokenModifier.static;
							}
							if (modifiers & ts.ModifierFlags.Async) {
								modifierSet |= 1 << TokenModifier.async;
							}
							if ((modifiers & ts.ModifierFlags.Readonly) || (nodeFlags & ts.NodeFlags.Const) || (symbol.getFlags() & ts.SymbolFlags.EnumMember)) {
								modifierSet |= 1 << TokenModifier.readonly;
							}
							if ((typeIdx === TokenType.variable || typeIdx === TokenType.function) && isLocalDeclaration(decl, sourceFile)) {
								modifierSet |= 1 << TokenModifier.local;
							}
							if (program.isSourceFileDefaultLibrary(decl.getSourceFile())) {
								modifierSet |= 1 << TokenModifier.defaultLibrary;
							}
						}

						collector(node, typeIdx, modifierSet);

					}
				}
			}
			ts.forEachChild(node, visit);

			inJSXElement = prevInJSXElement;
		}
		visit(sourceFile);
	}

	function classifySymbol(symbol: ts.Symbol, meaning: SemanticMeaning): TokenType | undefined {
		const flags = symbol.getFlags();
		if (flags & ts.SymbolFlags.Class) {
			return TokenType.class;
		} else if (flags & ts.SymbolFlags.Enum) {
			return TokenType.enum;
		} else if (flags & ts.SymbolFlags.TypeAlias) {
			return TokenType.type;
		} else if (flags & ts.SymbolFlags.Interface) {
			if (meaning & SemanticMeaning.Type) {
				return TokenType.interface;
			}
		} else if (flags & ts.SymbolFlags.TypeParameter) {
			return TokenType.typeParameter;
		}
		let decl = symbol.valueDeclaration || symbol.declarations && symbol.declarations[0];
		if (decl && ts.isBindingElement(decl)) {
			decl = getDeclarationForBindingElement(<ts.BindingElement>decl);
		}
		return decl && tokenFromDeclarationMapping[decl.kind];
	}

	function reclassifyByType(typeChecker: ts.TypeChecker, node: ts.Node, typeIdx: TokenType): TokenType {
		// type based classifications
		if (typeIdx === TokenType.variable || typeIdx === TokenType.property || typeIdx === TokenType.parameter) {
			const type = typeChecker.getTypeAtLocation(node);
			if (type) {
				const test = (condition: (type: ts.Type) => boolean) => {
					return condition(type) || type.isUnion() && type.types.some(condition);
				}
				if (typeIdx !== TokenType.parameter && test(t => t.getConstructSignatures().length > 0)) {
					return TokenType.class;
				}
				if (test(t => t.getCallSignatures().length > 0) && !test(t => t.getProperties().length > 0) || isExpressionInCallExpression(node)) {
					return typeIdx === TokenType.property ? TokenType.member : TokenType.function;
				}
			}
		}
		return typeIdx;
	}

	function isLocalDeclaration(decl: ts.Declaration, sourceFile: ts.SourceFile): boolean {
		if (ts.isBindingElement(decl)) {
			decl = getDeclarationForBindingElement(decl);
		}
		if (ts.isVariableDeclaration(decl)) {
			return (!ts.isSourceFile(decl.parent.parent.parent) || ts.isCatchClause(decl.parent)) && decl.getSourceFile() === sourceFile;
		} else if (ts.isFunctionDeclaration(decl)) {
			return !ts.isSourceFile(decl.parent) && decl.getSourceFile() === sourceFile;
		}
		return false;
	}

	function getDeclarationForBindingElement(element: ts.BindingElement): ts.VariableDeclaration | ts.ParameterDeclaration {
		while (true) {
			if (ts.isBindingElement(element.parent.parent)) {
				element = element.parent.parent;
			} else {
				return element.parent.parent;
			}
		}
	}

	function inImportClause(node: ts.Node): boolean {
		const parent = node.parent;
		return parent && (ts.isImportClause(parent) || ts.isImportSpecifier(parent) || ts.isNamespaceImport(parent));
	}

	function isExpressionInCallExpression(node: ts.Node): boolean {
		while (isRightSideOfQualifiedNameOrPropertyAccess(node)) {
			node = node.parent
		}
		return ts.isCallExpression(node.parent) && node.parent.expression === node;
	}

	function isRightSideOfQualifiedNameOrPropertyAccess(node: ts.Node): boolean {
		return (ts.isQualifiedName(node.parent) && node.parent.right === node) || (ts.isPropertyAccessExpression(node.parent) && node.parent.name === node);
	}

	const enum SemanticMeaning {
		None = 0x0,
		Value = 0x1,
		Type = 0x2,
		Namespace = 0x4,
		All = Value | Type | Namespace
	}

	function getMeaningFromLocation(node: ts.Node): SemanticMeaning {
		const f = (<any>ts).getMeaningFromLocation;
		if (typeof f === 'function') {
			return f(node);
		}
		return SemanticMeaning.All;
	}

	const tokenFromDeclarationMapping: { [name: string]: TokenType } = {
		[ts.SyntaxKind.VariableDeclaration]: TokenType.variable,
		[ts.SyntaxKind.Parameter]: TokenType.parameter,
		[ts.SyntaxKind.PropertyDeclaration]: TokenType.property,
		[ts.SyntaxKind.ModuleDeclaration]: TokenType.namespace,
		[ts.SyntaxKind.EnumDeclaration]: TokenType.enum,
		[ts.SyntaxKind.EnumMember]: TokenType.enumMember,
		[ts.SyntaxKind.ClassDeclaration]: TokenType.class,
		[ts.SyntaxKind.MethodDeclaration]: TokenType.member,
		[ts.SyntaxKind.FunctionDeclaration]: TokenType.function,
		[ts.SyntaxKind.FunctionExpression]: TokenType.function,
		[ts.SyntaxKind.MethodSignature]: TokenType.member,
		[ts.SyntaxKind.GetAccessor]: TokenType.property,
		[ts.SyntaxKind.PropertySignature]: TokenType.property,
		[ts.SyntaxKind.InterfaceDeclaration]: TokenType.interface,
		[ts.SyntaxKind.TypeAliasDeclaration]: TokenType.type,
		[ts.SyntaxKind.TypeParameter]: TokenType.typeParameter,
		[ts.SyntaxKind.PropertyAssignment]: TokenType.property,
		[ts.SyntaxKind.ShorthandPropertyAssignment]: TokenType.property
	};

	return {
		create(info: ts.server.PluginCreateInfo) {
			return decorate(info.languageService, info.project.projectService.logger);
		},
		onConfigurationChanged(_config: any) {
		},
		// added for testing
		decorate(languageService: ts.LanguageService): ts.LanguageService {
			return decorate(languageService);
		}
	};
};

