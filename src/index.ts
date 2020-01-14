/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

const REQUIRED_MAJOR_VERSION = 3;
const REQUIRED_MINOR_VERSION = 7;

export = function init(modules: { typescript: typeof import("typescript/lib/tsserverlibrary") }) {
	console.log('typescript-vscode-sh-plugin initialized');
	const ts = modules.typescript;

	function hasVersion(requiredMajor: number, requiredMinor: number) {
		const parts = ts.version.split('.');
		const majorVersion = Number(parts[0]);
		return majorVersion < requiredMajor || ((majorVersion === requiredMajor) && requiredMinor <= Number(parts[1]));
	}

	function decorate(languageService: ts.LanguageService) {

		const intercept: Partial<ts.LanguageService> = Object.create(null);

		if (!hasVersion(REQUIRED_MAJOR_VERSION, REQUIRED_MINOR_VERSION)) {
			console.log(`typescript-vscode-sh-plugin not active, version ${REQUIRED_MAJOR_VERSION}.${REQUIRED_MINOR_VERSION} required, is ${ts.version}`);
			return languageService;
		}
		console.log(`Intercepting getEncodedSemanticClassifications and getEncodedSyntacticClassifications.`);

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

	const tokenFromDeclarationMapping: { [name: string]: TokenType } = {
		[ts.SyntaxKind.VariableDeclaration]: TokenType.variable,
		[ts.SyntaxKind.Parameter]: TokenType.parameter,
		[ts.SyntaxKind.PropertyDeclaration]: TokenType.property,
		[ts.SyntaxKind.ModuleDeclaration]: TokenType.namespace,
		[ts.SyntaxKind.EnumDeclaration]: TokenType.enum,
		[ts.SyntaxKind.EnumMember]: TokenType.property,
		[ts.SyntaxKind.ClassDeclaration]: TokenType.class,
		[ts.SyntaxKind.MethodDeclaration]: TokenType.member,
		[ts.SyntaxKind.FunctionDeclaration]: TokenType.function,
		[ts.SyntaxKind.MethodSignature]: TokenType.member,
		[ts.SyntaxKind.GetAccessor]: TokenType.property,
		[ts.SyntaxKind.PropertySignature]: TokenType.property,
		[ts.SyntaxKind.InterfaceDeclaration]: TokenType.interface,
		[ts.SyntaxKind.TypeAliasDeclaration]: TokenType.type,
		[ts.SyntaxKind.TypeParameter]: TokenType.typeParameter
	};

	function getSemanticTokens(jsLanguageService: ts.LanguageService, fileName: string, span: ts.TextSpan): number[] {
		let resultTokens: number[] = [];

		const program = jsLanguageService.getProgram();
		if (program) {
			const typeChecker = program.getTypeChecker();

			function visit(node: ts.Node) {
				if (!node || !ts.textSpanIntersectsWith(span, node.pos, node.getFullWidth())) {
					return;
				}
				if (ts.isIdentifier(node)) {
					const symbol = typeChecker.getSymbolAtLocation(node);
					if (symbol) {
						let typeIdx = classifySymbol(symbol);
						if (typeIdx !== undefined) {
							let modifierSet = 0;
							if (node.parent) {
								const parentTypeIdx = tokenFromDeclarationMapping[node.parent.kind];
								if (parentTypeIdx === typeIdx && (<ts.NamedDeclaration>node.parent).name === node) {
									modifierSet = TokenModifier.declaration;
								}
							}
							const decl = symbol.valueDeclaration;
							const modifiers = decl ? ts.getCombinedModifierFlags(decl) : 0;
							const nodeFlags = decl ? ts.getCombinedNodeFlags(decl) : 0;
							if (modifiers & ts.ModifierFlags.Static) {
								modifierSet |= TokenModifier.static;
							}
							if (modifiers & ts.ModifierFlags.Async) {
								modifierSet |= TokenModifier.async;
							}
							if ((modifiers & ts.ModifierFlags.Readonly) || (nodeFlags & ts.NodeFlags.Const) || (symbol.getFlags() & ts.SymbolFlags.EnumMember)) {
								modifierSet |= TokenModifier.readonly;
							}
							resultTokens.push(node.getStart(), node.getWidth(), typeIdx + modifierSet);
						}

					}
				}
				ts.forEachChild(node, visit);
			}
			const sourceFile = program.getSourceFile(fileName);
			if (sourceFile) {
				visit(sourceFile);
			}
		}

		return resultTokens;
	}

	function classifySymbol(symbol: ts.Symbol) {

		const flags = symbol.getFlags();
		if (flags & ts.SymbolFlags.Class) {
			return TokenType.class;
		} else if (flags & ts.SymbolFlags.Enum) {
			return TokenType.enum;
		} else if (flags & ts.SymbolFlags.TypeAlias) {
			return TokenType.type;
		} else if (flags & ts.SymbolFlags.Type) {
			if (flags & ts.SymbolFlags.Interface) {
				return TokenType.interface;
			} if (flags & ts.SymbolFlags.TypeParameter) {
				return TokenType.typeParameter;
			}
		}
		const decl = symbol.valueDeclaration || symbol.declarations && symbol.declarations[0];
		return decl && tokenFromDeclarationMapping[decl.kind];
	}

	return {
		create(info: ts.server.PluginCreateInfo) {
			return decorate(info.languageService);
		},
		onConfigurationChanged(_config: any) {
		},
	};
};

const enum TokenType {
	'class' = 0x100,
	'enum' = 0x200,
	'interface' = 0x300,
	'namespace' = 0x400,
	'typeParameter' = 0x500,
	'type' = 0x600,
	'parameter' = 0x700,
	'variable' = 0x800,
	'property' = 0x900,
	'constant' = 0xA00,
	'function' = 0xB00,
	'member' = 0xC00
}

const enum TokenModifier {
	'declaration' = 0x01,
	'static' = 0x02,
	'async' = 0x04,
	'readonly' = 0x08
}
