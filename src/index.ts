/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

import * as ts from 'typescript/lib/tsserverlibrary';

export = function init(/*{ typescript }: { typescript: typeof ts_module }*/) {
	console.log('ts-sh activated');
	return {
		create(info: ts.server.PluginCreateInfo) {
			return decorate(info.languageService);
		},
		onConfigurationChanged(_config: any) {
		},
	};
};

function decorate(languageService: ts.LanguageService) {

	const intercept: Partial<ts.LanguageService> = Object.create(null);

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
	//https://ts-ast-viewer.com/#code/AQ0g2CmAuwGbALzAJwG4BQZQGNwEMBnQ4AQQEYBmYAb2C22zgEtJwATJVTRxgcwD27AQAp8AGmAAjAJS0A9POB8+7NQ168oscAJz5wANXwAnLug2bsJmAFcTAO2XAA1MHyvgu-UdOeWbOw8ViAAvpagocBAA

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
					const decl = symbol.valueDeclaration || symbol.declarations && symbol.declarations[0];
					if (decl) {
						let typeIdx = tokenFromDeclarationMapping[decl.kind];
						if (typeIdx) {
							let modifierSet = 0;
							if (node.parent) {
								const parentTypeIdx = tokenFromDeclarationMapping[node.parent.kind];
								if (parentTypeIdx === typeIdx && (<ts.NamedDeclaration>node.parent).name === node) {
									modifierSet = TokenModifier.declaration;
								}
							}
							const modifiers = ts.getCombinedModifierFlags(decl);
							if (modifiers & ts.ModifierFlags.Static) {
								modifierSet |= TokenModifier.static;
							}
							if (modifiers & ts.ModifierFlags.Async) {
								modifierSet |= TokenModifier.async;
							}
							if (typeIdx !== undefined) {
								resultTokens.push(node.getStart(), node.getWidth(), typeIdx + modifierSet);
							}
						}
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

const enum TokenType {
	'class' = 0x1000,
	'enum' = 0x2000,
	'interface' = 0x3000,
	'namespace' = 0x4000,
	'typeParameter' = 0x5000,
	'type' = 0x6000,
	'parameter' = 0x7000,
	'variable' = 0x8000,
	'property' = 0x9000,
	'constant' = 0xA000,
	'function' = 0xB000,
	'member' = 0xC000,
}

const enum TokenModifier {
	'declaration' = 0x0001,
	'static' = 0x0002,
	'async' = 0x0004
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
