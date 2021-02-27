/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

import { TokenType, TokenModifier, TokenEncodingConsts } from './constants';

export function init(ts: typeof import("typescript/lib/tsserverlibrary")) {

	const enum SemanticMeaning {
		None = 0x0,
		Value = 0x1,
		Type = 0x2,
		Namespace = 0x4,
		All = Value | Type | Namespace
	}

	const enum CanUseLocals {
		False = 0,
		True = 1,
	}

	const enum IsCallExpression {
		False = 0,
		True = 1
	}

	const enum IsRightSideOfExpression {
		False = 0,
		True = 1
	}

	const valueSymbolFlags = (
		ts.SymbolFlags.FunctionScopedVariable
		| ts.SymbolFlags.BlockScopedVariable
		| ts.SymbolFlags.Property
		| ts.SymbolFlags.Function
	);

	const emptyLocals: ts.SymbolTable = <ts.SymbolTable><unknown>new Map<ts.__String, ts.Symbol>();

	// Using internal TS API:
	interface NodeWithLocals {
		locals: ts.SymbolTable | undefined;
	}
	interface NodeWithSymbol {
		symbol: ts.Symbol | undefined;
	}
	interface ProgramWithDiagnosticsProducingTypeChecker {
		getDiagnosticsProducingTypeChecker(): ts.TypeChecker;
	}
	interface SymbolWithExportSymbol {
		exportSymbol: ts.Symbol | undefined;
	}

	class Context {

		private readonly program: ts.Program;
		private readonly typeChecker: ts.TypeChecker;
		public readonly sourceFile: ts.SourceFile;

		private readonly locals: ts.SymbolTable[];

		private readonly scopeSymbolCacheStack: Map<string, ts.Symbol | null>[];
		private scopeSymbolCache: Map<string, ts.Symbol | null>;

		private readonly thisSymbolStack: (ts.Symbol | undefined)[];
		private thisSymbol: ts.Symbol | undefined;

		private readonly symbolToEncodedTokenType: Map<ts.Symbol, number>;

		public readonly result: number[];
		public resultLen: number;

		constructor(program: ts.Program, sourceFile: ts.SourceFile) {
			this.program = program;
			this.typeChecker = (<ProgramWithDiagnosticsProducingTypeChecker><unknown>this.program).getDiagnosticsProducingTypeChecker();
			this.sourceFile = sourceFile;
			this.locals = [];
			this.scopeSymbolCacheStack = [new Map<string, ts.Symbol | null>()];
			this.scopeSymbolCache = this.scopeSymbolCacheStack[this.scopeSymbolCacheStack.length - 1];
			this.thisSymbolStack = [undefined];
			this.thisSymbol = this.thisSymbolStack[this.thisSymbolStack.length - 1];
			this.symbolToEncodedTokenType = new Map<ts.Symbol, number>();
			this.result = [];
			this.resultLen = 0;
		}

		public findSymbolInLocals(escapedText: ts.__String, meaning: SemanticMeaning): ts.Symbol | null {
			const locals = this.locals;
			for (let i = locals.length - 1; i >= 0; i--) {
				const symbolTable = locals[i];
				const symbol = symbolTable.get(escapedText);
				if (symbol) {
					if (((meaning & SemanticMeaning.Value) === 0) && (symbol.getFlags() & valueSymbolFlags)) {
						// we are not looking for Value
						continue;
					}
					const symbolWithExportSymbol = (<SymbolWithExportSymbol><unknown>symbol);
					if (symbolWithExportSymbol.exportSymbol) {
						return symbolWithExportSymbol.exportSymbol;
					}
					return symbol;
				}
			}
			return null;
		}

		public pushResult(node: ts.Identifier, tokenType: TokenType, modifiers: number): void {
			const nodeWidth = node.getWidth();
			if (nodeWidth === 0) {
				return;
			}
			const result = this.result;
			let resultLen = this.resultLen;
			result[resultLen++] = node.getStart();
			result[resultLen++] = nodeWidth;
			result[resultLen++] = ((tokenType + 1) << TokenEncodingConsts.typeOffset) + modifiers;
			this.resultLen = resultLen;
		}

		public pushEncodedResult(node: ts.Identifier, encodedToken: number): void {
			const nodeWidth = node.getWidth();
			if (nodeWidth === 0) {
				return;
			}
			const result = this.result;
			let resultLen = this.resultLen;
			result[resultLen++] = node.getStart();
			result[resultLen++] = nodeWidth;
			result[resultLen++] = encodedToken;
			this.resultLen = resultLen;
		}

		public pushResultFromSymbolCache(symbol: ts.Symbol, node: ts.Identifier, extraTokenModifiers: number, isCallExpression: IsCallExpression, isRightSideOfExpression: IsRightSideOfExpression): boolean {
			const cachedEncodedTokenType = this.symbolToEncodedTokenType.get(symbol);
			if (typeof cachedEncodedTokenType !== 'undefined') {
				let tokenType = (cachedEncodedTokenType >>> TokenEncodingConsts.typeOffset) - 1;
				let tokenModifiers = (cachedEncodedTokenType & TokenEncodingConsts.modifierMask);
				if (symbol.valueDeclaration && symbol.valueDeclaration.kind === ts.SyntaxKind.Parameter && symbol.valueDeclaration.parent.kind === ts.SyntaxKind.Constructor) {
					// handle the case of a property declaration in constructor
					if (isRightSideOfExpression === IsRightSideOfExpression.True) {
						if (tokenType === TokenType.parameter) {
							tokenType = TokenType.property;
						} else if (tokenType === TokenType.function) {
							tokenType = TokenType.member;
						}
					} else {
						if (tokenType === TokenType.property) {
							tokenType = TokenType.parameter;
						} else if (tokenType === TokenType.member) {
							tokenType = TokenType.function;
						}
					}
				}
				if (isCallExpression === IsCallExpression.True && (tokenType === TokenType.variable || tokenType === TokenType.property || tokenType === TokenType.parameter)) {
					tokenType = tokenType === TokenType.property ? TokenType.member : TokenType.function;
				}
				tokenModifiers = tokenModifiers | extraTokenModifiers;
				this.pushResult(node, tokenType, tokenModifiers);
				return true;
			}
			return false;
		}

		public saveResultInSymbolCache(symbol: ts.Symbol, encodedTokenType: number): void {
			this.symbolToEncodedTokenType.set(symbol, encodedTokenType);
		}

		public pushThisSymbol(node_scope_this: ts.Symbol | undefined): void {
			this.thisSymbolStack.push(node_scope_this);
			this.thisSymbol = this.thisSymbolStack[this.thisSymbolStack.length - 1];
		}

		public popThisSymbol(): void {
			this.thisSymbolStack.pop();
			this.thisSymbol = this.thisSymbolStack[this.thisSymbolStack.length - 1];
		}

		public getThisSymbol(): ts.Symbol | undefined {
			return this.thisSymbol;
		}

		public pushLocals(nodeLocals: ts.SymbolTable | undefined): void {
			if (!nodeLocals) {
				this.locals.push(emptyLocals);
				return;
			}
			// if any aliases are present, make a new symbol table
			let hasAlias = false;
			nodeLocals.forEach((symbol) => {
				if (hasAlias) {
					return;
				}
				hasAlias = (symbol.flags & ts.SymbolFlags.Alias) ? true : false;
			});

			if (hasAlias) {
				// must recreate symbol table with resolved aliases
				let newSymbolTable = new Map<ts.__String, ts.Symbol>();
				nodeLocals.forEach((symbol, key) => {
					if (symbol.flags & ts.SymbolFlags.Alias) {
						newSymbolTable.set(key, this.typeChecker.getAliasedSymbol(symbol));
					} else {
						newSymbolTable.set(key, symbol);
					}
				});
				this.locals.push(<ts.SymbolTable><unknown>newSymbolTable);
			} else {
				this.locals.push(nodeLocals);
			}
		}

		public popLocals(): void {
			this.locals.pop();
		}

		public pushScope(nodeLocals: ts.SymbolTable | undefined): void {
			this.scopeSymbolCacheStack.push(new Map<string, ts.Symbol | null>());
			this.scopeSymbolCache = this.scopeSymbolCacheStack[this.scopeSymbolCacheStack.length - 1];
			this.pushLocals(nodeLocals);
		}

		public popScope(): void {
			this.scopeSymbolCacheStack.pop();
			this.scopeSymbolCache = this.scopeSymbolCacheStack[this.scopeSymbolCacheStack.length - 1];
			this.popLocals();
		}

		public getCachedScopeSymbol(cacheKey: string): ts.Symbol | null | undefined {
			return this.scopeSymbolCache.get(cacheKey);
		}

		public setCachedScopeSymbol(cacheKey: string, symbol: ts.Symbol | null): void {
			this.scopeSymbolCache.set(cacheKey, symbol);
		}

		public getTypeCheckerSymbolAtLocation(node: ts.Node): ts.Symbol | undefined {
			const symbol = this.typeChecker.getSymbolAtLocation(node);
			if (symbol && symbol.flags & ts.SymbolFlags.Alias) {
				return this.typeChecker.getAliasedSymbol(symbol);
			}
			return symbol;
		}

		public reclassifyWithType(node: ts.Node, tokenType: TokenType.variable | TokenType.property | TokenType.parameter): TokenType {
			// type based classifications
			const type = this.typeChecker.getTypeAtLocation(node);
			if (type) {
				if (tokenType !== TokenType.parameter && typeHasConstructSignatures(type)) {
					return TokenType.class;
				}
				if (typeHasCallSignatures(type) && !typeHasProperties(type)) {
					return tokenType === TokenType.property ? TokenType.member : TokenType.function;
				}
			}
			return tokenType;
		}

		public isSourceFileDefaultLibrary(sourceFile: ts.SourceFile): boolean {
			return this.program.isSourceFileDefaultLibrary(sourceFile);
		}
	}

	function getSymbolFast(ctx: Context, node: ts.Identifier, meaning: SemanticMeaning, canUseLocals: CanUseLocals): ts.Symbol | null {
		const escapedText = node.escapedText;
		if (canUseLocals === CanUseLocals.True) {
			const symbolInLocals = ctx.findSymbolInLocals(escapedText, meaning);
			if (symbolInLocals) {
				return symbolInLocals;
			}
		}
		const cacheKey = `${meaning}${escapedText}`;
		const symbolInCache = ctx.getCachedScopeSymbol(cacheKey);
		if (typeof symbolInCache !== 'undefined') {
			return symbolInCache;
		}
		const symbol = ctx.getTypeCheckerSymbolAtLocation(node) || null;
		ctx.setCachedScopeSymbol(cacheKey, symbol);
		return symbol;
	}

	function nodeModifiersToTokenModifiers(modifiers: ts.ModifiersArray | undefined): number {
		if (!modifiers) {
			return 0;
		}
		let result = 0;
		for (const modifier of modifiers) {
			if (modifier.kind === ts.SyntaxKind.StaticKeyword) {
				result |= 1 << TokenModifier.static;
			} else if (modifier.kind === ts.SyntaxKind.AsyncKeyword) {
				result |= 1 << TokenModifier.async;
			} else if (modifier.kind === ts.SyntaxKind.ReadonlyKeyword) {
				result |= 1 << TokenModifier.readonly;
			}
		}
		return result;
	}

	function canCacheSymbol(symbol: ts.Symbol | undefined): boolean {
		// can only cache symbols that have 1 declaration or that have multiple declarations of the same kind
		if (!symbol || !symbol.declarations || symbol.declarations.length <= 1) {
			return true;
		}
		const kind = symbol.declarations[0].kind;
		for (let i = 1, len = symbol.declarations.length; i < len; i++) {
			if (symbol.declarations[i].kind !== kind) {
				return false;
			}
		}
		return true;
	}

	function visitIdentifierInDeclaration(ctx: Context, node: ts.Identifier, symbol: ts.Symbol | undefined, _meaning: SemanticMeaning, tokenType: TokenType, tokenModifiers: number): void {
		const canCache = canCacheSymbol(symbol);
		if (canCache && symbol && ctx.pushResultFromSymbolCache(symbol, node, (1 << TokenModifier.declaration), IsCallExpression.False, IsRightSideOfExpression.False)) {
			return;
		}

		if (tokenType === TokenType.variable || tokenType === TokenType.property || tokenType === TokenType.parameter) {
			tokenType = ctx.reclassifyWithType(node, tokenType);
			if (tokenType !== TokenType.variable && tokenType !== TokenType.function) {
				// the token has changed its type, be sure to remove local
				tokenModifiers &= ~(1 << TokenModifier.local);
			}
		}

		const encodedTokenType = ((tokenType + 1) << TokenEncodingConsts.typeOffset) | tokenModifiers;
		if (canCache && symbol) {
			ctx.saveResultInSymbolCache(symbol, encodedTokenType);
		}
		ctx.pushEncodedResult(node, encodedTokenType | (1 << TokenModifier.declaration));
	}

	function visitIdentifierWithSymbol(ctx: Context, node: ts.Identifier, symbol: ts.Symbol, meaning: SemanticMeaning, isCallExpression: IsCallExpression, isRightSideOfExpression: IsRightSideOfExpression): void {
		let canCache = canCacheSymbol(symbol);
		if (canCache && ctx.pushResultFromSymbolCache(symbol, node, 0, isCallExpression, isRightSideOfExpression)) {
			return;
		}

		const flags = symbol.getFlags();
		let decl = symbol.valueDeclaration || (symbol.declarations && symbol.declarations[0]);
		let tokenType: TokenType;
		if (flags & ts.SymbolFlags.Class) {
			tokenType = TokenType.class;
		} else if (flags & ts.SymbolFlags.Enum) {
			tokenType = TokenType.enum;
		} else if (flags & ts.SymbolFlags.TypeAlias) {
			tokenType = TokenType.type;
		} else if ((flags & ts.SymbolFlags.Interface) && (meaning & SemanticMeaning.Type)) {
			tokenType = TokenType.interface;
		} else if (flags & ts.SymbolFlags.TypeParameter) {
			tokenType = TokenType.typeParameter;
		} else if (decl) {
			if (decl.kind === ts.SyntaxKind.BindingElement) {
				decl = findBindingElementParentDeclaration(<ts.BindingElement>decl);
			}
			switch (decl.kind) {
				case ts.SyntaxKind.VariableDeclaration: tokenType = TokenType.variable; break;
				case ts.SyntaxKind.Parameter:
					// handle the case of a property declaration in constructor
					if (isRightSideOfExpression === IsRightSideOfExpression.True) {
						tokenType = TokenType.property;
					} else {
						tokenType = TokenType.parameter;
					}
					break;
				case ts.SyntaxKind.PropertyDeclaration: tokenType = TokenType.property; break;
				case ts.SyntaxKind.ModuleDeclaration: tokenType = TokenType.namespace; break;
				case ts.SyntaxKind.EnumDeclaration: tokenType = TokenType.enum; break;
				case ts.SyntaxKind.EnumMember: tokenType = TokenType.enumMember; break;
				case ts.SyntaxKind.ClassDeclaration: tokenType = TokenType.class; break;
				case ts.SyntaxKind.MethodDeclaration: tokenType = TokenType.member; break;
				case ts.SyntaxKind.FunctionDeclaration: tokenType = TokenType.function; break;
				case ts.SyntaxKind.FunctionExpression: tokenType = TokenType.function; break;
				case ts.SyntaxKind.MethodSignature: tokenType = TokenType.member; break;
				case ts.SyntaxKind.GetAccessor: tokenType = TokenType.property; break;
				case ts.SyntaxKind.PropertySignature: tokenType = TokenType.property; break;
				case ts.SyntaxKind.InterfaceDeclaration: tokenType = TokenType.interface; break;
				case ts.SyntaxKind.TypeAliasDeclaration: tokenType = TokenType.type; break;
				case ts.SyntaxKind.TypeParameter: tokenType = TokenType.typeParameter; break;
				case ts.SyntaxKind.PropertyAssignment: tokenType = TokenType.property; break;
				case ts.SyntaxKind.ShorthandPropertyAssignment: tokenType = TokenType.property; break;
				default:
					return;
			}
		} else {
			return;
		}

		const isDefaultLibrary = ctx.isSourceFileDefaultLibrary(decl.getSourceFile());

		if (tokenType === TokenType.variable || tokenType === TokenType.property || tokenType === TokenType.parameter) {
			if (isCallExpression === IsCallExpression.True && !isDefaultLibrary) {
				// normally, classes cannot be called, but there are default library symbols like String which can appear in a call expression
				tokenType = tokenType === TokenType.property ? TokenType.member : TokenType.function;
				canCache = false;
			} else {
				tokenType = ctx.reclassifyWithType(node, tokenType);
			}
		}

		let modifierSet = 0;
		const valueDecl = symbol.valueDeclaration;
		if (valueDecl) {
			const modifiers = ts.getCombinedModifierFlags(valueDecl);
			const nodeFlags = ts.getCombinedNodeFlags(valueDecl);
			if (modifiers & ts.ModifierFlags.Static) {
				modifierSet |= 1 << TokenModifier.static;
			}
			if (modifiers & ts.ModifierFlags.Async) {
				modifierSet |= 1 << TokenModifier.async;
			}
			if (tokenType !== TokenType.interface) {
				if ((modifiers & ts.ModifierFlags.Readonly) || (nodeFlags & ts.NodeFlags.Const) || (flags & ts.SymbolFlags.EnumMember)) {
					modifierSet |= 1 << TokenModifier.readonly;
				}
			}
			if ((tokenType === TokenType.variable || tokenType === TokenType.function) && isLocalDeclaration(valueDecl, ctx.sourceFile)) {
				modifierSet |= 1 << TokenModifier.local;
			}
		}

		if (isDefaultLibrary) {
			modifierSet |= 1 << TokenModifier.defaultLibrary;
		}

		const encodedTokenType = ((tokenType + 1) << TokenEncodingConsts.typeOffset) | modifierSet;
		if (canCache) {
			ctx.saveResultInSymbolCache(symbol, encodedTokenType);
		}
		ctx.pushEncodedResult(node, encodedTokenType);
	}
	function visitIdentifier(ctx: Context, node: ts.Identifier, meaning: SemanticMeaning, canUseLocals: CanUseLocals, isCallExpression: IsCallExpression): ts.Symbol | null {
		const symbol = getSymbolFast(ctx, node, meaning, canUseLocals);
		if (!symbol) {
			return null;
		}
		visitIdentifierWithSymbol(ctx, node, symbol, meaning, isCallExpression, IsRightSideOfExpression.False);
		return symbol;
	}
	function visitPropertyName(ctx: Context, node: ts.StringLiteral | ts.NumericLiteral | ts.ComputedPropertyName): void {
		if (node.kind === ts.SyntaxKind.ComputedPropertyName) {
			visitExpression(ctx, node.expression);
		}
	}
	function visitQualifiedName(ctx: Context, node: ts.QualifiedName, meaning: SemanticMeaning): ts.Symbol | null {
		return visitQualifiedNameWithSplitMeaning(ctx, node, meaning, meaning);
	}
	function visitQualifiedNameWithSplitMeaning(ctx: Context, node: ts.QualifiedName, meaningLeft: SemanticMeaning, meaningRight: SemanticMeaning): ts.Symbol | null {
		const leftSymbol = visitEntityName(ctx, node.left, meaningLeft);
		if (!leftSymbol || !leftSymbol.exports) {
			return null;
		}
		const symbol = leftSymbol.exports.get(node.right.escapedText);
		if (!symbol) {
			return null;
		}
		visitIdentifierWithSymbol(ctx, node.right, symbol, meaningRight, IsCallExpression.False, IsRightSideOfExpression.False);
		return symbol;
	}
	function visitEntityName(ctx: Context, node: ts.EntityName, meaning: SemanticMeaning): ts.Symbol | null {
		switch (node.kind) {
			case ts.SyntaxKind.Identifier:
				return visitIdentifier(ctx, node, meaning, CanUseLocals.True, IsCallExpression.False);
			case ts.SyntaxKind.QualifiedName:
				return visitQualifiedName(ctx, node, meaning);
		}
	}
	function visitParameterDeclaration(ctx: Context, node: ts.ParameterDeclaration): ts.Symbol | null | void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		visitBindingName(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, TokenType.parameter, tokenModifiers);
		let typeSymbol: ts.Symbol | null | void;
		if (node.type) {
			typeSymbol = visitTypeNode(ctx, node.type);
		}
		if (node.initializer) {
			visitExpression(ctx, node.initializer);
		}
		return typeSymbol;
	}
	function visitParameters(ctx: Context, nodes: ts.NodeArray<ts.ParameterDeclaration>): void {
		for (const parameter of nodes) {
			visitParameterDeclaration(ctx, parameter);
		}
	}
	function visitFunctionTypeNode(ctx: Context, node: ts.FunctionTypeNode): ts.Symbol | null | void {
		visitDecorators(ctx, node.decorators);
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		visitTypeNode(ctx, node.type);
		return (<NodeWithSymbol><unknown>node).symbol;
	}
	function visitConstructorTypeNode(ctx: Context, node: ts.ConstructorTypeNode): ts.Symbol | null | void {
		visitDecorators(ctx, node.decorators);
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		visitTypeNode(ctx, node.type);
		return (<NodeWithSymbol><unknown>node).symbol;
	}
	function visitImportTypeNode(ctx: Context, node: ts.ImportTypeNode): void {
		visitTypeNode(ctx, node.argument);
		if (node.qualifier) {
			visitEntityName(ctx, node.qualifier, (node.isTypeOf ? SemanticMeaning.Value : SemanticMeaning.Type));
		}
		visitTypeArguments(ctx, node.typeArguments);
	}
	function visitTypeReference(ctx: Context, node: ts.TypeReferenceNode): ts.Symbol | null {
		let symbol: ts.Symbol | null;
		if (node.typeName.kind === ts.SyntaxKind.Identifier) {
			symbol = visitIdentifier(ctx, node.typeName, SemanticMeaning.Type, CanUseLocals.True, IsCallExpression.False);
		} else {
			symbol = visitQualifiedNameWithSplitMeaning(ctx, node.typeName, SemanticMeaning.Namespace, SemanticMeaning.Type);
		}
		visitTypeArguments(ctx, node.typeArguments);
		return symbol;
	}
	function visitExpressionWithTypeArguments(ctx: Context, node: ts.ExpressionWithTypeArguments): void {
		if (node.parent.kind === ts.SyntaxKind.HeritageClause && (node.parent.token === ts.SyntaxKind.ExtendsKeyword || node.parent.token === ts.SyntaxKind.ImplementsKeyword)) {
			if (node.expression.kind === ts.SyntaxKind.Identifier) {
				visitIdentifier(ctx, <ts.Identifier>node.expression, SemanticMeaning.Type, CanUseLocals.True, IsCallExpression.False);
			} else {
				visitExpression(ctx, node.expression);
			}
		} else {
			visitExpression(ctx, node.expression);
		}
		visitTypeArguments(ctx, node.typeArguments);
	}
	function visitTypePredicateNode(ctx: Context, node: ts.TypePredicateNode): void {
		if (node.parameterName.kind === ts.SyntaxKind.Identifier) {
			visitIdentifier(ctx, node.parameterName, SemanticMeaning.Value, CanUseLocals.True, IsCallExpression.False);
		}
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
	}
	function visitTypeQueryNode(ctx: Context, node: ts.TypeQueryNode): void {
		visitEntityName(ctx, node.exprName, SemanticMeaning.Value);
	}
	function visitTypeLiteralNode(ctx: Context, node: ts.TypeLiteralNode): void {
		for (const member of node.members) {
			switch (member.kind) {
				case ts.SyntaxKind.CallSignature:
					visitCallSignatureDeclaration(ctx, <ts.CallSignatureDeclaration>member);
					break;
				case ts.SyntaxKind.ConstructSignature:
					visitConstructSignatureDeclaration(ctx, <ts.ConstructSignatureDeclaration>member);
					break;
				case ts.SyntaxKind.PropertySignature:
					visitPropertySignature(ctx, <ts.PropertySignature>member);
					break;
				case ts.SyntaxKind.MethodSignature:
					visitMethodSignature(ctx, <ts.MethodSignature>member);
					break;
				case ts.SyntaxKind.IndexSignature:
					visitIndexSignatureDeclaration(ctx, <ts.IndexSignatureDeclaration>member);
					break;
			}
		}
	}
	function visitArrayTypeNode(ctx: Context, node: ts.ArrayTypeNode): void {
		visitTypeNode(ctx, node.elementType);
	}
	function visitTupleTypeNode(ctx: Context, node: ts.TupleTypeNode): void {
		for (const elementType of node.elementTypes) {
			visitTypeNode(ctx, elementType);
		}
	}
	function visitOptionalTypeNode(ctx: Context, node: ts.OptionalTypeNode): void {
		visitTypeNode(ctx, node.type);
	}
	function visitRestTypeNode(ctx: Context, node: ts.RestTypeNode): void {
		visitTypeNode(ctx, node.type);
	}
	function visitUnionTypeNode(ctx: Context, node: ts.UnionTypeNode): void {
		for (const type of node.types) {
			visitTypeNode(ctx, type);
		}
	}
	function visitIntersectionTypeNode(ctx: Context, node: ts.IntersectionTypeNode): void {
		for (const type of node.types) {
			visitTypeNode(ctx, type);
		}
	}
	function visitConditionalTypeNode(ctx: Context, node: ts.ConditionalTypeNode): void {
		visitTypeNode(ctx, node.checkType);
		visitTypeNode(ctx, node.extendsType);
		visitTypeNode(ctx, node.trueType);
		visitTypeNode(ctx, node.falseType);
	}
	function visitInferTypeNode(ctx: Context, node: ts.InferTypeNode): void {
		visitTypeParameterDeclaration(ctx, node.typeParameter);
	}
	function visitParenthesizedTypeNode(ctx: Context, node: ts.ParenthesizedTypeNode): ts.Symbol | null | void {
		return visitTypeNode(ctx, node.type);
	}
	function visitTypeOperatorNode(ctx: Context, node: ts.TypeOperatorNode): void {
		visitTypeNode(ctx, node.type);
	}
	function visitIndexedAccessTypeNode(ctx: Context, node: ts.IndexedAccessTypeNode): void {
		visitTypeNode(ctx, node.objectType);
		visitTypeNode(ctx, node.indexType);
	}
	function visitMappedTypeNode(ctx: Context, node: ts.MappedTypeNode): void {
		visitTypeParameterDeclaration(ctx, node.typeParameter);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
	}
	function visitTypeNode(ctx: Context, node: ts.TypeNode): ts.Symbol | null | void {
		switch (node.kind) {
			case ts.SyntaxKind.FunctionType:
				return visitFunctionTypeNode(ctx, <ts.FunctionTypeNode>node);
			case ts.SyntaxKind.ConstructorType:
				return visitConstructorTypeNode(ctx, <ts.ConstructorTypeNode>node);
			case ts.SyntaxKind.ImportType:
				return visitImportTypeNode(ctx, <ts.ImportTypeNode>node);
			case ts.SyntaxKind.TypeReference:
				return visitTypeReference(ctx, <ts.TypeReferenceNode>node);
			case ts.SyntaxKind.ExpressionWithTypeArguments:
				return visitExpressionWithTypeArguments(ctx, <ts.ExpressionWithTypeArguments>node);
			case ts.SyntaxKind.TypePredicate:
				return visitTypePredicateNode(ctx, <ts.TypePredicateNode>node);
			case ts.SyntaxKind.TypeQuery:
				return visitTypeQueryNode(ctx, <ts.TypeQueryNode>node);
			case ts.SyntaxKind.TypeLiteral:
				return visitTypeLiteralNode(ctx, <ts.TypeLiteralNode>node);
			case ts.SyntaxKind.ArrayType:
				return visitArrayTypeNode(ctx, <ts.ArrayTypeNode>node);
			case ts.SyntaxKind.TupleType:
				return visitTupleTypeNode(ctx, <ts.TupleTypeNode>node);
			case ts.SyntaxKind.OptionalType:
				return visitOptionalTypeNode(ctx, <ts.OptionalTypeNode>node);
			case ts.SyntaxKind.RestType:
				return visitRestTypeNode(ctx, <ts.RestTypeNode>node);
			case ts.SyntaxKind.UnionType:
				return visitUnionTypeNode(ctx, <ts.UnionTypeNode>node);
			case ts.SyntaxKind.IntersectionType:
				return visitIntersectionTypeNode(ctx, <ts.IntersectionTypeNode>node);
			case ts.SyntaxKind.ConditionalType:
				return visitConditionalTypeNode(ctx, <ts.ConditionalTypeNode>node);
			case ts.SyntaxKind.InferType:
				return visitInferTypeNode(ctx, <ts.InferTypeNode>node);
			case ts.SyntaxKind.ParenthesizedType:
				return visitParenthesizedTypeNode(ctx, <ts.ParenthesizedTypeNode>node);
			case ts.SyntaxKind.TypeOperator:
				return visitTypeOperatorNode(ctx, <ts.TypeOperatorNode>node);
			case ts.SyntaxKind.IndexedAccessType:
				return visitIndexedAccessTypeNode(ctx, <ts.IndexedAccessTypeNode>node);
			case ts.SyntaxKind.MappedType:
				return visitMappedTypeNode(ctx, <ts.MappedTypeNode>node);
			// ignoring:
			case ts.SyntaxKind.ThisKeyword:
				return ctx.getThisSymbol();
			// case SyntaxKind.JSDocTypeExpression:
			// case ts.SyntaxKind.JSDocAllType:
			// case ts.SyntaxKind.JSDocUnknownType:
			// case ts.SyntaxKind.JSDocNonNullableType:
			// case ts.SyntaxKind.JSDocNullableType:
			// case ts.SyntaxKind.JSDocOptionalType:
			// case ts.SyntaxKind.JSDocFunctionType:
			// case ts.SyntaxKind.JSDocVariadicType:
			// case ts.SyntaxKind.JSDocNamepathType:
			// case ts.SyntaxKind.JSDocSignature:
			// case ts.SyntaxKind.JSDocTypeLiteral:
			// case ts.SyntaxKind.LiteralType:
			// case ts.SyntaxKind.ThisType:
			// case ts.SyntaxKind.NullKeyword:
			// case ts.SyntaxKind.TrueKeyword
			// case ts.SyntaxKind.FalseKeyword:
			// case ts.SyntaxKind.AnyKeyword:
			// case ts.SyntaxKind.UnknownKeyword:
			// case ts.SyntaxKind.NumberKeyword:
			// case ts.SyntaxKind.BigIntKeyword:
			// case ts.SyntaxKind.ObjectKeyword:
			// case ts.SyntaxKind.BooleanKeyword:
			// case ts.SyntaxKind.StringKeyword:
			// case ts.SyntaxKind.SymbolKeyword:
			// case ts.SyntaxKind.ThisKeyword:
			// case ts.SyntaxKind.VoidKeyword:
			// case ts.SyntaxKind.UndefinedKeyword:
			// case ts.SyntaxKind.NullKeyword:
			// case ts.SyntaxKind.NeverKeyword:
		}
	}
	function visitTypeArguments(ctx: Context, nodes: ts.NodeArray<ts.TypeNode> | undefined): void {
		if (nodes) {
			for (const node of nodes) {
				visitTypeNode(ctx, node);
			}
		}
	}
	function visitPrefixUnaryExpression(ctx: Context, node: ts.PrefixUnaryExpression): void {
		visitExpression(ctx, node.operand);
	}
	function visitPostfixUnaryExpression(ctx: Context, node: ts.PostfixUnaryExpression): void {
		visitExpression(ctx, node.operand);
	}
	function visitPartiallyEmittedExpression(ctx: Context, node: ts.PartiallyEmittedExpression): void {
		visitExpression(ctx, node.expression);
	}
	function visitFunctionExpression(ctx: Context, node: ts.FunctionExpression): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name) {
			ctx.pushResult(node.name, TokenType.function, tokenModifiers | (1 << TokenModifier.declaration));
		}
		visitTypeParameters(ctx, node.typeParameters);
		const new_scope_this = visitParametersAndGetThisSymbol(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.body) {
			ctx.pushThisSymbol(new_scope_this);
			ctx.pushScope((<NodeWithLocals><unknown>node).locals);
			visitBlock(ctx, node.body);
			ctx.popScope();
			ctx.popThisSymbol();
		}
	}
	function visitTemplateSpan(ctx: Context, node: ts.TemplateSpan): void {
		visitExpression(ctx, node.expression);
	}
	function visitTemplateExpression(ctx: Context, node: ts.TemplateExpression): void {
		// ignoring node.head
		for (const templateSpan of node.templateSpans) {
			visitTemplateSpan(ctx, templateSpan);
		}
	}
	function visitParenthesizedExpression(ctx: Context, node: ts.ParenthesizedExpression): ts.Symbol | null | void {
		return visitExpression(ctx, node.expression);
	}
	function visitArrayLiteralExpression(ctx: Context, node: ts.ArrayLiteralExpression): void {
		for (const element of node.elements) {
			visitExpression(ctx, element);
		}
	}
	function visitMethodDeclaration(ctx: Context, node: ts.MethodDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name.kind === ts.SyntaxKind.Identifier) {
			ctx.pushResult(node.name, TokenType.member, tokenModifiers | (1 << TokenModifier.declaration));
		} else {
			visitPropertyName(ctx, node.name);
		}
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.body) {
			ctx.pushScope((<NodeWithLocals><unknown>node).locals);
			visitBlock(ctx, node.body);
			ctx.popScope();
		}
	}
	function visitGetAccessorDeclaration(ctx: Context, node: ts.GetAccessorDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name.kind === ts.SyntaxKind.Identifier) {
			visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Value, TokenType.property, tokenModifiers);
		} else {
			visitPropertyName(ctx, node.name);
		}
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.body) {
			ctx.pushScope((<NodeWithLocals><unknown>node).locals);
			visitBlock(ctx, node.body);
			ctx.popScope();
		}
	}
	function visitSetAccessorDeclaration(ctx: Context, node: ts.SetAccessorDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name.kind === ts.SyntaxKind.Identifier) {
			visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Value, TokenType.property, tokenModifiers);
		} else {
			visitPropertyName(ctx, node.name);
		}
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.body) {
			ctx.pushScope((<NodeWithLocals><unknown>node).locals);
			visitBlock(ctx, node.body);
			ctx.popScope();
		}
	}
	function visitPropertyAssignment(ctx: Context, node: ts.PropertyAssignment): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name.kind === ts.SyntaxKind.Identifier) {
			visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Value, TokenType.property, tokenModifiers);
		} else {
			visitPropertyName(ctx, node.name);
		}
		visitExpression(ctx, node.initializer);
	}
	function visitShorthandPropertyAssignment(ctx: Context, node: ts.ShorthandPropertyAssignment): void {
		visitDecorators(ctx, node.decorators);
		const tokenType = ctx.reclassifyWithType(node.name, TokenType.property);
		ctx.pushResult(node.name, tokenType, (1 << TokenModifier.declaration));
		if (node.objectAssignmentInitializer) {
			visitExpression(ctx, node.objectAssignmentInitializer);
		}
	}
	function visitSpreadAssignment(ctx: Context, node: ts.SpreadAssignment): void {
		visitExpression(ctx, node.expression);
	}
	function visitObjectLiteralExpression(ctx: Context, node: ts.ObjectLiteralExpression): ts.Symbol | null | void {
		for (const property of node.properties) {
			switch (property.kind) {
				case ts.SyntaxKind.MethodDeclaration:
					visitMethodDeclaration(ctx, property);
					break;
				case ts.SyntaxKind.GetAccessor:
					visitGetAccessorDeclaration(ctx, property);
					break;
				case ts.SyntaxKind.SetAccessor:
					visitSetAccessorDeclaration(ctx, property);
					break;
				case ts.SyntaxKind.PropertyAssignment:
					visitPropertyAssignment(ctx, property);
					break;
				case ts.SyntaxKind.ShorthandPropertyAssignment:
					visitShorthandPropertyAssignment(ctx, property);
					break;
				case ts.SyntaxKind.SpreadAssignment:
					visitSpreadAssignment(ctx, property);
					break;
			}
		}
		return (<NodeWithSymbol><unknown>node).symbol;
	}
	function visitNewExpression(ctx: Context, node: ts.NewExpression): void {
		visitExpression(ctx, node.expression);
		visitTypeArguments(ctx, node.typeArguments);
		if (node.arguments) {
			for (const argument of node.arguments) {
				visitExpression(ctx, argument);
			}
		}
	}
	function visitMetaProperty(ctx: Context, node: ts.MetaProperty): void {
		visitIdentifier(ctx, node.name, SemanticMeaning.Value, CanUseLocals.False, IsCallExpression.False);
	}
	function visitPropertyDeclaration(ctx: Context, node: ts.PropertyDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const modifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name.kind === ts.SyntaxKind.Identifier) {
			visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Value, TokenType.property, modifiers)
		} else {
			visitPropertyName(ctx, node.name);
		}
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.initializer) {
			visitExpression(ctx, node.initializer);
		}
	}
	function visitConstructorDeclaration(ctx: Context, node: ts.ConstructorDeclaration): void {
		visitDecorators(ctx, node.decorators);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.body) {
			ctx.pushScope((<NodeWithLocals><unknown>node).locals);
			visitBlock(ctx, node.body);
			ctx.popScope();
		}
	}
	function visitClassElement(ctx: Context, node: ts.ClassElement): void {
		switch (node.kind) {
			case ts.SyntaxKind.PropertyDeclaration:
				return visitPropertyDeclaration(ctx, <ts.PropertyDeclaration>node);
			case ts.SyntaxKind.MethodDeclaration:
				return visitMethodDeclaration(ctx, <ts.MethodDeclaration>node);
			case ts.SyntaxKind.Constructor:
				return visitConstructorDeclaration(ctx, <ts.ConstructorDeclaration>node);
			case ts.SyntaxKind.GetAccessor:
				return visitGetAccessorDeclaration(ctx, <ts.GetAccessorDeclaration>node);
			case ts.SyntaxKind.SetAccessor:
				return visitSetAccessorDeclaration(ctx, <ts.SetAccessorDeclaration>node);
			case ts.SyntaxKind.IndexSignature:
				return visitIndexSignatureDeclaration(ctx, <ts.IndexSignatureDeclaration>node);
			// ignoring:
			// case ts.SyntaxKind.SemicolonClassElement:
		}
	}
	function visitClassExpression(ctx: Context, node: ts.ClassExpression): void {
		visitDecorators(ctx, node.decorators);
		if (node.name) {
			visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Value, TokenType.class, 0);
		}
		visitTypeParameters(ctx, node.typeParameters);
		if (node.heritageClauses) {
			for (const heritageClause of node.heritageClauses) {
				visitHeritageClause(ctx, heritageClause);
			}
		}
		ctx.pushThisSymbol((<NodeWithSymbol><unknown>node).symbol);
		for (const member of node.members) {
			visitClassElement(ctx, member);
		}
		ctx.popThisSymbol();
	}
	function visitPropertyAccessExpression(ctx: Context, node: ts.PropertyAccessExpression, isCallExpression: IsCallExpression): ts.Symbol | null | void {
		const leftSymbol = visitExpression(ctx, node.expression);
		if (leftSymbol && (leftSymbol.getFlags() & valueSymbolFlags) === 0 && (leftSymbol.exports || leftSymbol.members)) {
			let symbol: ts.Symbol | undefined;
			if (leftSymbol.exports) {
				symbol = leftSymbol.exports.get(node.name.escapedText);
			}
			if (!symbol && leftSymbol.members) {
				symbol = leftSymbol.members.get(node.name.escapedText);
			}
			if (symbol) {
				// found it!
				visitIdentifierWithSymbol(ctx, node.name, symbol, SemanticMeaning.Value, isCallExpression, IsRightSideOfExpression.True);
				return symbol;
			}
		}
		const symbol = ctx.getTypeCheckerSymbolAtLocation(node.name);
		if (symbol) {
			visitIdentifierWithSymbol(ctx, node.name, symbol, SemanticMeaning.Value, isCallExpression, IsRightSideOfExpression.True);
		}
		return symbol;
	}
	function visitElementAccessExpression(ctx: Context, node: ts.ElementAccessExpression): void {
		visitExpression(ctx, node.expression);
		visitExpression(ctx, node.argumentExpression);
	}
	function visitTaggedTemplateExpression(ctx: Context, node: ts.TaggedTemplateExpression): void {
		visitExpression(ctx, node.tag);
		visitTypeArguments(ctx, node.typeArguments);
		visitExpression(ctx, node.template);
	}
	function visitCallExpression(ctx: Context, node: ts.CallExpression): void {
		if (node.expression.kind === ts.SyntaxKind.PropertyAccessExpression) {
			visitPropertyAccessExpression(ctx, (<ts.PropertyAccessExpression>node.expression), IsCallExpression.True);
		} else if (node.expression.kind === ts.SyntaxKind.Identifier) {
			visitIdentifier(ctx, <ts.Identifier>node.expression, SemanticMeaning.Value, CanUseLocals.True, IsCallExpression.True);
		} else {
			visitExpression(ctx, node.expression);
		}
		visitTypeArguments(ctx, node.typeArguments);
		for (const argument of node.arguments) {
			visitExpression(ctx, argument);
		}
	}
	function visitNonNullExpression(ctx: Context, node: ts.NonNullExpression): void {
		visitExpression(ctx, node.expression);
	}
	function visitDeleteExpression(ctx: Context, node: ts.DeleteExpression): void {
		visitExpression(ctx, node.expression);
	}
	function visitTypeOfExpression(ctx: Context, node: ts.TypeOfExpression): void {
		visitExpression(ctx, node.expression);
	}
	function visitVoidExpression(ctx: Context, node: ts.VoidExpression): void {
		visitExpression(ctx, node.expression);
	}
	function visitAwaitExpression(ctx: Context, node: ts.AwaitExpression): void {
		visitExpression(ctx, node.expression);
	}
	function visitTypeAssertion(ctx: Context, node: ts.TypeAssertion): ts.Symbol | null | void {
		const symbol = visitTypeNode(ctx, node.type);
		visitExpression(ctx, node.expression);
		return symbol;
	}
	function visitYieldExpression(ctx: Context, node: ts.YieldExpression): void {
		if (node.expression) {
			visitExpression(ctx, node.expression);
		}
	}
	function visitBinaryExpression(ctx: Context, node: ts.BinaryExpression): void {
		visitExpression(ctx, node.left);
		visitExpression(ctx, node.right);
	}
	function visitConditionalExpression(ctx: Context, node: ts.ConditionalExpression): void {
		visitExpression(ctx, node.condition);
		visitExpression(ctx, node.whenTrue);
		visitExpression(ctx, node.whenFalse);
	}
	function visitArrowFunction(ctx: Context, node: ts.ArrowFunction): void {
		visitDecorators(ctx, node.decorators);
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}

		ctx.pushScope((<NodeWithLocals><unknown>node).locals);
		if (node.body.kind === ts.SyntaxKind.Block) {
			visitBlock(ctx, <ts.Block>node.body);
		} else {
			// must be expression
			visitExpression(ctx, <ts.Expression>node.body);
		}
		ctx.popScope();
	}
	function visitSpreadElement(ctx: Context, node: ts.SpreadElement): void {
		visitExpression(ctx, node.expression);
	}
	function visitAsExpression(ctx: Context, node: ts.AsExpression): ts.Symbol | null | void {
		visitExpression(ctx, node.expression);
		return visitTypeNode(ctx, node.type);
	}
	function visitCommaListExpression(ctx: Context, node: ts.CommaListExpression): void {
		for (const element of node.elements) {
			visitExpression(ctx, element);
		}
	}
	function visitJsxAttributes(ctx: Context, node: ts.JsxAttributes): void {
		for (const property of node.properties) {
			if (property.kind === ts.SyntaxKind.JsxAttribute) {
				// ignoring property.name
				if (property.initializer && property.initializer.kind === ts.SyntaxKind.JsxExpression) {
					visitJsxExpression(ctx, property.initializer);
				}
			}
			// ignoring JsxSpreadAttribute
		}
	}
	function visitJsxChild(ctx: Context, node: ts.JsxChild): void {
		switch (node.kind) {
			case ts.SyntaxKind.JsxElement:
				return visitJsxElement(ctx, node);
			case ts.SyntaxKind.JsxSelfClosingElement:
				return visitJsxSelfClosingElement(ctx, node);
			case ts.SyntaxKind.JsxFragment:
				return visitJsxFragment(ctx, node);
			case ts.SyntaxKind.JsxExpression:
				return visitJsxExpression(ctx, node);
			// ignoring:
			// case ts.SyntaxKind.JsxText:
		}
	}
	function visitJsxElement(ctx: Context, node: ts.JsxElement): void {
		visitJsxOpeningElement(ctx, node.openingElement);
		for (const child of node.children) {
			visitJsxChild(ctx, child);
		}
		// ignoring node.closingElement
	}
	function visitJsxSelfClosingElement(ctx: Context, node: ts.JsxSelfClosingElement): void {
		// ignoring node.tagName;
		// ignoring node.typeArguments;
		visitJsxAttributes(ctx, node.attributes);
	}
	function visitJsxFragment(ctx: Context, node: ts.JsxFragment): void {
		for (const child of node.children) {
			visitJsxChild(ctx, child);
		}
	}
	function visitJsxOpeningElement(ctx: Context, node: ts.JsxOpeningElement): void {
		// ignoring node.tagName;
		// ignoring node.typeArguments;
		visitJsxAttributes(ctx, node.attributes);
	}
	function visitJsxExpression(ctx: Context, node: ts.JsxExpression): void {
		if (node.expression) {
			visitExpression(ctx, node.expression);
		}
	}
	function visitExpression(ctx: Context, node: ts.Expression): ts.Symbol | null | void {
		switch (node.kind) {
			case ts.SyntaxKind.PrefixUnaryExpression:
				return visitPrefixUnaryExpression(ctx, <ts.PrefixUnaryExpression>node);
			case ts.SyntaxKind.PostfixUnaryExpression:
				return visitPostfixUnaryExpression(ctx, <ts.PostfixUnaryExpression>node);
			case ts.SyntaxKind.PartiallyEmittedExpression:
				return visitPartiallyEmittedExpression(ctx, <ts.PartiallyEmittedExpression>node);
			case ts.SyntaxKind.Identifier:
				return visitIdentifier(ctx, <ts.Identifier>node, SemanticMeaning.Value, CanUseLocals.True, IsCallExpression.False);
			case ts.SyntaxKind.FunctionExpression:
				return visitFunctionExpression(ctx, <ts.FunctionExpression>node);
			case ts.SyntaxKind.TemplateExpression:
				return visitTemplateExpression(ctx, <ts.TemplateExpression>node);
			case ts.SyntaxKind.ParenthesizedExpression:
				return visitParenthesizedExpression(ctx, <ts.ParenthesizedExpression>node);
			case ts.SyntaxKind.ArrayLiteralExpression:
				return visitArrayLiteralExpression(ctx, <ts.ArrayLiteralExpression>node);
			case ts.SyntaxKind.ObjectLiteralExpression:
				return visitObjectLiteralExpression(ctx, <ts.ObjectLiteralExpression>node);
			case ts.SyntaxKind.NewExpression:
				return visitNewExpression(ctx, <ts.NewExpression>node);
			case ts.SyntaxKind.MetaProperty:
				return visitMetaProperty(ctx, <ts.MetaProperty>node);
			case ts.SyntaxKind.ClassExpression:
				return visitClassExpression(ctx, <ts.ClassExpression>node);
			case ts.SyntaxKind.PropertyAccessExpression:
				return visitPropertyAccessExpression(ctx, <ts.PropertyAccessExpression>node, IsCallExpression.False);
			case ts.SyntaxKind.ElementAccessExpression:
				return visitElementAccessExpression(ctx, <ts.ElementAccessExpression>node);
			case ts.SyntaxKind.TaggedTemplateExpression:
				return visitTaggedTemplateExpression(ctx, <ts.TaggedTemplateExpression>node);
			case ts.SyntaxKind.CallExpression:
				return visitCallExpression(ctx, <ts.CallExpression>node);
			case ts.SyntaxKind.NonNullExpression:
				return visitNonNullExpression(ctx, <ts.NonNullExpression>node);
			case ts.SyntaxKind.DeleteExpression:
				return visitDeleteExpression(ctx, <ts.DeleteExpression>node);
			case ts.SyntaxKind.TypeOfExpression:
				return visitTypeOfExpression(ctx, <ts.TypeOfExpression>node);
			case ts.SyntaxKind.VoidExpression:
				return visitVoidExpression(ctx, <ts.VoidExpression>node);
			case ts.SyntaxKind.AwaitExpression:
				return visitAwaitExpression(ctx, <ts.AwaitExpression>node);
			case ts.SyntaxKind.TypeAssertionExpression:
				return visitTypeAssertion(ctx, <ts.TypeAssertion>node);
			case ts.SyntaxKind.YieldExpression:
				return visitYieldExpression(ctx, <ts.YieldExpression>node);
			case ts.SyntaxKind.BinaryExpression:
				return visitBinaryExpression(ctx, <ts.BinaryExpression>node);
			case ts.SyntaxKind.ConditionalExpression:
				return visitConditionalExpression(ctx, <ts.ConditionalExpression>node);
			case ts.SyntaxKind.ArrowFunction:
				return visitArrowFunction(ctx, <ts.ArrowFunction>node);
			case ts.SyntaxKind.SpreadElement:
				return visitSpreadElement(ctx, <ts.SpreadElement>node);
			case ts.SyntaxKind.AsExpression:
				return visitAsExpression(ctx, <ts.AsExpression>node);
			case ts.SyntaxKind.CommaListExpression:
				return visitCommaListExpression(ctx, <ts.CommaListExpression>node);
			case ts.SyntaxKind.JsxAttributes:
				return visitJsxAttributes(ctx, <ts.JsxAttributes>node);
			case ts.SyntaxKind.JsxElement:
				return visitJsxElement(ctx, <ts.JsxElement>node);
			case ts.SyntaxKind.JsxSelfClosingElement:
				return visitJsxSelfClosingElement(ctx, <ts.JsxSelfClosingElement>node);
			case ts.SyntaxKind.JsxFragment:
				return visitJsxFragment(ctx, <ts.JsxFragment>node);
			case ts.SyntaxKind.JsxOpeningElement:
				return visitJsxOpeningElement(ctx, <ts.JsxOpeningElement>node);
			case ts.SyntaxKind.JsxExpression:
				return visitJsxExpression(ctx, <ts.JsxExpression>node);
			// ignoring:
			case ts.SyntaxKind.ThisKeyword:
				return ctx.getThisSymbol();
			// case ts.SyntaxKind.JsxOpeningFragment:
			// case ts.SyntaxKind.JsxClosingFragment:
			// case ts.SyntaxKind.OmittedExpression:
			// case ts.SyntaxKind.SyntheticExpression:
			// case ts.SyntaxKind.NullKeyword:
			// case ts.SyntaxKind.TrueKeyword:
			// case ts.SyntaxKind.FalseKeyword:
			// case ts.SyntaxKind.SuperKeyword:
			// case ts.SyntaxKind.ImportKeyword:
			// case ts.SyntaxKind.StringLiteral:
			// case ts.SyntaxKind.RegularExpressionLiteral:
			// case ts.SyntaxKind.NoSubstitutionTemplateLiteral:
			// case ts.SyntaxKind.NumericLiteral:
			// case ts.SyntaxKind.BigIntLiteral:
		}
	}
	function visitDecorator(ctx: Context, node: ts.Decorator): void {
		visitExpression(ctx, node.expression);
	}
	function visitDecorators(ctx: Context, decorators: ts.NodeArray<ts.Decorator> | undefined): void {
		if (decorators) {
			for (const decorator of decorators) {
				visitDecorator(ctx, decorator);
			}
		}
	}
	function visitParametersAndGetThisSymbol(ctx: Context, parameters: ts.NodeArray<ts.ParameterDeclaration>): ts.Symbol | undefined {
		let isFirst = true;
		let result: ts.Symbol | undefined = undefined;
		for (const parameter of parameters) {
			if (isFirst) {
				isFirst = false;
				if (parameter.name.kind === ts.SyntaxKind.Identifier && parameter.name.getText() === 'this') {
					result = visitParameterDeclaration(ctx, parameter) || undefined;
				} else {
					visitParameterDeclaration(ctx, parameter);
				}
			} else {
				visitParameterDeclaration(ctx, parameter);
			}
		}
		return result;
	}
	function visitFunctionDeclaration(ctx: Context, node: ts.FunctionDeclaration, parentKind: ts.SyntaxKind): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name) {
			ctx.pushResult(node.name, TokenType.function, tokenModifiers | (1 << TokenModifier.declaration) | (parentKind !== ts.SyntaxKind.SourceFile ? (1 << TokenModifier.local) : 0));
		}
		visitTypeParameters(ctx, node.typeParameters);
		const new_scope_this = visitParametersAndGetThisSymbol(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.body) {
			ctx.pushThisSymbol(new_scope_this);
			ctx.pushScope((<NodeWithLocals><unknown>node).locals);
			visitBlock(ctx, node.body);
			ctx.popScope();
			ctx.popThisSymbol();
		}
	}
	function visitClassDeclaration(ctx: Context, node: ts.ClassDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name) {
			visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Value | SemanticMeaning.Type, TokenType.class, tokenModifiers);
		}
		visitTypeParameters(ctx, node.typeParameters);
		if (node.heritageClauses) {
			for (const heritageClause of node.heritageClauses) {
				visitHeritageClause(ctx, heritageClause);
			}
		}
		ctx.pushThisSymbol((<NodeWithSymbol><unknown>node).symbol);
		for (const member of node.members) {
			visitClassElement(ctx, member);
		}
		ctx.popThisSymbol();
	}
	function visitMissingDeclaration(ctx: Context, node: ts.MissingDeclaration): void {
		visitDecorators(ctx, node.decorators);
	}
	function visitTypeParameterDeclaration(ctx: Context, node: ts.TypeParameterDeclaration): void {
		visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Type, TokenType.typeParameter, 0);
		if (node.constraint) {
			visitTypeNode(ctx, node.constraint);
		}
		if (node.default) {
			visitTypeNode(ctx, node.default);
		}
		if (node.expression) {
			visitExpression(ctx, node.expression);
		}
	}
	function visitTypeParameters(ctx: Context, nodes: ts.NodeArray<ts.TypeParameterDeclaration> | undefined): void {
		if (nodes) {
			for (const typeParameter of nodes) {
				visitTypeParameterDeclaration(ctx, typeParameter);
			}
		}
	}
	function visitHeritageClause(ctx: Context, node: ts.HeritageClause): void {
		for (const type of node.types) {
			visitExpressionWithTypeArguments(ctx, type);
		}
	}
	function visitCallSignatureDeclaration(ctx: Context, node: ts.CallSignatureDeclaration): void {
		visitDecorators(ctx, node.decorators);
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
	}
	function visitConstructSignatureDeclaration(ctx: Context, node: ts.ConstructSignatureDeclaration): void {
		visitDecorators(ctx, node.decorators);
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
	}
	function visitPropertySignature(ctx: Context, node: ts.PropertySignature): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name.kind === ts.SyntaxKind.Identifier) {
			visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Value, TokenType.property, tokenModifiers);
		} else {
			visitPropertyName(ctx, node.name);
		}
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.initializer) {
			visitExpression(ctx, node.initializer);
		}
	}
	function visitMethodSignature(ctx: Context, node: ts.MethodSignature): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name.kind === ts.SyntaxKind.Identifier) {
			visitIdentifierInDeclaration(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, SemanticMeaning.Value, TokenType.member, tokenModifiers);
		} else {
			visitPropertyName(ctx, node.name);
		}
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
	}
	function visitIndexSignatureDeclaration(ctx: Context, node: ts.IndexSignatureDeclaration): void {
		visitDecorators(ctx, node.decorators);
		visitTypeParameters(ctx, node.typeParameters);
		visitParameters(ctx, node.parameters);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
	}
	function visitInterfaceDeclaration(ctx: Context, node: ts.InterfaceDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		ctx.pushResult(node.name, TokenType.interface, tokenModifiers | (1 << TokenModifier.declaration));
		visitTypeParameters(ctx, node.typeParameters);
		if (node.heritageClauses) {
			for (const heritageClause of node.heritageClauses) {
				visitHeritageClause(ctx, heritageClause);
			}
		}
		for (const member of node.members) {
			switch (member.kind) {
				case ts.SyntaxKind.CallSignature:
					visitCallSignatureDeclaration(ctx, <ts.CallSignatureDeclaration>member);
					break;
				case ts.SyntaxKind.ConstructSignature:
					visitConstructSignatureDeclaration(ctx, <ts.ConstructSignatureDeclaration>member);
					break;
				case ts.SyntaxKind.PropertySignature:
					visitPropertySignature(ctx, <ts.PropertySignature>member);
					break;
				case ts.SyntaxKind.MethodSignature:
					visitMethodSignature(ctx, <ts.MethodSignature>member);
					break;
				case ts.SyntaxKind.IndexSignature:
					visitIndexSignatureDeclaration(ctx, <ts.IndexSignatureDeclaration>member);
					break;
			}
		}
	}
	function visitTypeAliasDeclaration(ctx: Context, node: ts.TypeAliasDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		ctx.pushResult(node.name, TokenType.type, tokenModifiers | (1 << TokenModifier.declaration));
		visitTypeParameters(ctx, node.typeParameters);
		visitTypeNode(ctx, node.type);
	}
	function visitEnumDeclaration(ctx: Context, node: ts.EnumDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		ctx.pushResult(node.name, TokenType.enum, tokenModifiers | (1 << TokenModifier.declaration));

		const symbol = (<NodeWithSymbol><unknown>node).symbol;
		ctx.pushLocals(symbol ? symbol.exports : undefined);
		for (const enumMember of node.members) {
			if (enumMember.name.kind === ts.SyntaxKind.Identifier) {
				ctx.pushResult(enumMember.name, TokenType.enumMember, (1 << TokenModifier.declaration) | (1 << TokenModifier.readonly));
			} else {
				visitPropertyName(ctx, enumMember.name);
			}
			if (enumMember.initializer) {
				visitExpression(ctx, enumMember.initializer);
			}
		}
		ctx.popLocals();
	}
	function visitModuleBlock(ctx: Context, node: ts.ModuleBlock): void {
		for (const statement of node.statements) {
			visitStatement(ctx, statement, ts.SyntaxKind.ModuleBlock);
		}
	}
	function visitModuleDeclaration(ctx: Context, node: ts.ModuleDeclaration): void {
		visitDecorators(ctx, node.decorators);
		const tokenModifiers = nodeModifiersToTokenModifiers(node.modifiers);
		if (node.name.kind === ts.SyntaxKind.Identifier) {
			ctx.pushResult(node.name, TokenType.namespace, tokenModifiers | (1 << TokenModifier.declaration));
		}
		if (node.body) {
			ctx.pushScope((<NodeWithLocals><unknown>node).locals);
			switch (node.body.kind) {
				case ts.SyntaxKind.ModuleBlock:
					visitModuleBlock(ctx, node.body);
					break;
				case ts.SyntaxKind.ModuleDeclaration:
					visitModuleDeclaration(ctx, node.body);
					break;
				// ignoring:
				// case ts.SyntaxKind.Identifier:
			}
			ctx.popScope();
		}
	}
	function visitExternalModuleReference(ctx: Context, node: ts.ExternalModuleReference): void {
		if (node.expression.kind === ts.SyntaxKind.Identifier) {
			visitIdentifier(ctx, <ts.Identifier>node.expression, SemanticMeaning.All, CanUseLocals.True, IsCallExpression.False);
		} else {
			visitExpression(ctx, node.expression);
		}
	}
	function visitImportEqualsDeclaration(ctx: Context, node: ts.ImportEqualsDeclaration): void {
		visitDecorators(ctx, node.decorators);
		ctx.pushResult(node.name, TokenType.namespace, 0);
		if (node.moduleReference.kind === ts.SyntaxKind.ExternalModuleReference) {
			visitExternalModuleReference(ctx, node.moduleReference);
		} else {
			// isInternalModuleImportEqualsDeclaration
			//     import a = |b|; // Namespace
			//     import a = |b.c|; // Value, type, namespace
			//     import a = |b.c|.d; // Namespace
			if (node.moduleReference.kind === ts.SyntaxKind.Identifier) {
				visitIdentifier(ctx, node.moduleReference, SemanticMeaning.Namespace, CanUseLocals.True, IsCallExpression.False);
			} else {
				visitQualifiedNameWithSplitMeaning(ctx, node.moduleReference, SemanticMeaning.Namespace, SemanticMeaning.All);
			}
		}
	}
	function visitNamespaceExportDeclaration(ctx: Context, node: ts.NamespaceExportDeclaration): void {
		if (node.name) {
			visitIdentifier(ctx, node.name, SemanticMeaning.Namespace | SemanticMeaning.Value, CanUseLocals.True, IsCallExpression.False);
		}
	}
	function visitExportSpecifier(ctx: Context, node: ts.ExportSpecifier): void {
		if (node.propertyName) {
			visitIdentifier(ctx, node.propertyName, SemanticMeaning.Value, CanUseLocals.True, IsCallExpression.False);
		}
		visitIdentifier(ctx, node.name, SemanticMeaning.Value, CanUseLocals.True, IsCallExpression.False);
	}
	function visitNamedExports(ctx: Context, node: ts.NamedExports): void {
		for (const element of node.elements) {
			visitExportSpecifier(ctx, element);
		}
	}
	function visitExportDeclaration(ctx: Context, node: ts.ExportDeclaration): void {
		visitDecorators(ctx, node.decorators);
		if (node.exportClause) {
			visitNamedExports(ctx, node.exportClause);
		}
		if (node.moduleSpecifier) {
			visitExpression(ctx, node.moduleSpecifier);
		}
	}
	function visitExportAssignment(ctx: Context, node: ts.ExportAssignment): void {
		visitDecorators(ctx, node.decorators);
		if (node.expression.kind === ts.SyntaxKind.Identifier) {
			visitIdentifier(ctx, <ts.Identifier>node.expression, SemanticMeaning.All, CanUseLocals.True, IsCallExpression.False);
		} else {
			visitExpression(ctx, node.expression);
		}
	}
	function visitBlock(ctx: Context, node: ts.Block): void {
		ctx.pushScope((<NodeWithLocals><unknown>node).locals);
		for (const statement of node.statements) {
			visitStatement(ctx, statement, ts.SyntaxKind.Block);
		}
		ctx.popScope();
	}
	function visitBindingName(ctx: Context, node: ts.BindingName, symbol: ts.Symbol | undefined, tokenType: TokenType.variable | TokenType.parameter, tokenModifiers: number): void {
		switch (node.kind) {
			case ts.SyntaxKind.Identifier:
				visitIdentifierInDeclaration(ctx, node, symbol, SemanticMeaning.Value, tokenType, tokenModifiers);
				break;
			case ts.SyntaxKind.ObjectBindingPattern:
				visitObjectBindingPattern(ctx, node, tokenType, tokenModifiers);
				break;
			case ts.SyntaxKind.ArrayBindingPattern:
				visitArrayBindingPattern(ctx, node, tokenType, tokenModifiers);
				break;
		}

	}
	function visitBindingElement(ctx: Context, node: ts.BindingElement, tokenType: TokenType.variable | TokenType.parameter, tokenModifiers: number): void {
		visitDecorators(ctx, node.decorators);
		if (node.propertyName) {
			if (node.propertyName.kind === ts.SyntaxKind.Identifier) {
				visitIdentifier(ctx, node.propertyName, SemanticMeaning.Value, CanUseLocals.False, IsCallExpression.False);
			} else {
				visitPropertyName(ctx, node.propertyName);
			}
		}
		if (node.name) {
			visitBindingName(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, tokenType, tokenModifiers);
		}
		if (node.initializer) {
			visitExpression(ctx, node.initializer);
		}
	}
	function visitObjectBindingPattern(ctx: Context, node: ts.ObjectBindingPattern, tokenType: TokenType.variable | TokenType.parameter, tokenModifiers: number): void {
		for (const element of node.elements) {
			visitBindingElement(ctx, element, tokenType, tokenModifiers);
		}
	}
	function visitArrayBindingPattern(ctx: Context, node: ts.ArrayBindingPattern, tokenType: TokenType.variable | TokenType.parameter, tokenModifiers: number): void {
		for (const element of node.elements) {
			if (element.kind === ts.SyntaxKind.BindingElement) {
				visitBindingElement(ctx, element, tokenType, tokenModifiers);
			}
		}
	}
	function visitVariableDeclaration(ctx: Context, node: ts.VariableDeclaration, tokenModifiers: number): void {
		visitDecorators(ctx, node.decorators);
		visitBindingName(ctx, node.name, (<NodeWithSymbol><unknown>node).symbol, TokenType.variable, tokenModifiers);
		if (node.type) {
			visitTypeNode(ctx, node.type);
		}
		if (node.initializer) {
			visitExpression(ctx, node.initializer);
		}
	}
	function visitVariableDeclarationList(ctx: Context, node: ts.VariableDeclarationList, tokenModifiers: number): void {
		if (node.flags & ts.NodeFlags.Const) {
			tokenModifiers |= (1 << TokenModifier.readonly);
		}
		for (const decl of node.declarations) {
			visitVariableDeclaration(ctx, decl, tokenModifiers);
		}
	}
	function visitVariableStatement(ctx: Context, node: ts.VariableStatement, parentKind: ts.SyntaxKind): void {
		visitDecorators(ctx, node.decorators);
		let tokenModifiers = 0;
		if (parentKind !== ts.SyntaxKind.SourceFile) {
			tokenModifiers |= (1 << TokenModifier.local);
		}
		visitVariableDeclarationList(ctx, node.declarationList, tokenModifiers);
	}
	function visitExpressionStatement(ctx: Context, node: ts.ExpressionStatement): void {
		visitExpression(ctx, node.expression);
	}
	function visitIfStatement(ctx: Context, node: ts.IfStatement): void {
		visitExpression(ctx, node.expression);
		visitStatement(ctx, node.thenStatement, ts.SyntaxKind.IfStatement);
		if (node.elseStatement) {
			visitStatement(ctx, node.elseStatement, ts.SyntaxKind.IfStatement);
		}
	}
	function visitDoStatement(ctx: Context, node: ts.DoStatement): void {
		visitStatement(ctx, node.statement, ts.SyntaxKind.DoStatement);
		visitExpression(ctx, node.expression);
	}
	function visitWhileStatement(ctx: Context, node: ts.WhileStatement): void {
		visitExpression(ctx, node.expression);
		visitStatement(ctx, node.statement, ts.SyntaxKind.WhileStatement);
	}
	function visitForInitializer(ctx: Context, node: ts.ForInitializer): void {
		if (node.kind === ts.SyntaxKind.VariableDeclarationList) {
			visitVariableDeclarationList(ctx, <ts.VariableDeclarationList>node, 1 << TokenModifier.local);
		} else {
			visitExpression(ctx, node);
		}
	}
	function visitForStatement(ctx: Context, node: ts.ForStatement): void {
		ctx.pushScope((<NodeWithLocals><unknown>node).locals);
		if (node.initializer) {
			visitForInitializer(ctx, node.initializer);
		}
		if (node.condition) {
			visitExpression(ctx, node.condition);
		}
		if (node.incrementor) {
			visitExpression(ctx, node.incrementor);
		}
		visitStatement(ctx, node.statement, ts.SyntaxKind.ForOfStatement);
		ctx.popScope();
	}
	function visitForInStatement(ctx: Context, node: ts.ForInStatement): void {
		ctx.pushScope((<NodeWithLocals><unknown>node).locals);
		visitForInitializer(ctx, node.initializer);
		visitExpression(ctx, node.expression);
		visitStatement(ctx, node.statement, ts.SyntaxKind.ForOfStatement);
		ctx.popScope();
	}
	function visitForOfStatement(ctx: Context, node: ts.ForOfStatement): void {
		ctx.pushScope((<NodeWithLocals><unknown>node).locals);
		visitForInitializer(ctx, node.initializer);
		visitExpression(ctx, node.expression);
		visitStatement(ctx, node.statement, ts.SyntaxKind.ForOfStatement);
		ctx.popScope();
	}
	function visitCaseClause(ctx: Context, node: ts.CaseClause): void {
		visitExpression(ctx, node.expression);
		for (const statement of node.statements) {
			visitStatement(ctx, statement, ts.SyntaxKind.CaseClause);
		}
	}
	function visitDefaultClause(ctx: Context, node: ts.DefaultClause): void {
		for (const statement of node.statements) {
			visitStatement(ctx, statement, ts.SyntaxKind.CaseClause);
		}
	}
	function visitCaseBlock(ctx: Context, node: ts.CaseBlock): void {
		ctx.pushScope((<NodeWithLocals><unknown>node).locals);
		for (const clause of node.clauses) {
			if (clause.kind === ts.SyntaxKind.CaseClause) {
				visitCaseClause(ctx, clause);
			} else {
				visitDefaultClause(ctx, clause);
			}
		}
		ctx.popScope();
	}
	function visitSwitchStatement(ctx: Context, node: ts.SwitchStatement): void {
		visitExpression(ctx, node.expression);
		visitCaseBlock(ctx, node.caseBlock);
	}
	function visitLabeledStatement(ctx: Context, node: ts.LabeledStatement): void {
		// ignoring node.label
		visitStatement(ctx, node.statement, ts.SyntaxKind.LabeledStatement);
	}
	function visitThrowStatement(ctx: Context, node: ts.ThrowStatement): void {
		if (node.expression) {
			visitExpression(ctx, node.expression);
		}
	}
	function visitCatchClause(ctx: Context, node: ts.CatchClause): void {
		if (node.variableDeclaration) {
			visitVariableDeclaration(ctx, node.variableDeclaration, (1 << TokenModifier.local));
		}
		visitBlock(ctx, node.block);
	}
	function visitTryStatement(ctx: Context, node: ts.TryStatement): void {
		visitBlock(ctx, node.tryBlock);
		if (node.catchClause) {
			ctx.pushScope((<NodeWithLocals><unknown>node).locals);
			visitCatchClause(ctx, node.catchClause);
			ctx.popScope();
		}
		if (node.finallyBlock) {
			visitBlock(ctx, node.finallyBlock);
		}
	}
	function visitReturnStatement(ctx: Context, node: ts.ReturnStatement): void {
		if (node.expression) {
			visitExpression(ctx, node.expression);
		}
	}
	function visitStatement(ctx: Context, node: ts.Statement, parentKind: ts.SyntaxKind): void {
		switch (node.kind) {
			case ts.SyntaxKind.FunctionDeclaration:
				return visitFunctionDeclaration(ctx, <ts.FunctionDeclaration>node, parentKind);
			case ts.SyntaxKind.ClassDeclaration:
				return visitClassDeclaration(ctx, <ts.ClassDeclaration>node);
			case ts.SyntaxKind.MissingDeclaration:
				return visitMissingDeclaration(ctx, <ts.MissingDeclaration>node);
			case ts.SyntaxKind.InterfaceDeclaration:
				return visitInterfaceDeclaration(ctx, <ts.InterfaceDeclaration>node);
			case ts.SyntaxKind.TypeAliasDeclaration:
				return visitTypeAliasDeclaration(ctx, <ts.TypeAliasDeclaration>node);
			case ts.SyntaxKind.EnumDeclaration:
				return visitEnumDeclaration(ctx, <ts.EnumDeclaration>node);
			case ts.SyntaxKind.ModuleDeclaration:
				return visitModuleDeclaration(ctx, <ts.ModuleDeclaration>node);
			case ts.SyntaxKind.ImportEqualsDeclaration:
				return visitImportEqualsDeclaration(ctx, <ts.ImportEqualsDeclaration>node);
			case ts.SyntaxKind.NamespaceExportDeclaration:
				return visitNamespaceExportDeclaration(ctx, <ts.NamespaceExportDeclaration>node);
			case ts.SyntaxKind.ExportDeclaration:
				return visitExportDeclaration(ctx, <ts.ExportDeclaration>node);
			case ts.SyntaxKind.ExportAssignment:
				return visitExportAssignment(ctx, <ts.ExportAssignment>node);
			case ts.SyntaxKind.Block:
				return visitBlock(ctx, <ts.Block>node);
			case ts.SyntaxKind.VariableStatement:
				return visitVariableStatement(ctx, <ts.VariableStatement>node, parentKind);
			case ts.SyntaxKind.ExpressionStatement:
				return visitExpressionStatement(ctx, <ts.ExpressionStatement>node);
			case ts.SyntaxKind.IfStatement:
				return visitIfStatement(ctx, <ts.IfStatement>node);
			case ts.SyntaxKind.DoStatement:
				return visitDoStatement(ctx, <ts.DoStatement>node);
			case ts.SyntaxKind.WhileStatement:
				return visitWhileStatement(ctx, <ts.WhileStatement>node);
			case ts.SyntaxKind.ForStatement:
				return visitForStatement(ctx, <ts.ForStatement>node);
			case ts.SyntaxKind.ForInStatement:
				return visitForInStatement(ctx, <ts.ForInStatement>node);
			case ts.SyntaxKind.ForOfStatement:
				return visitForOfStatement(ctx, <ts.ForOfStatement>node);
			case ts.SyntaxKind.SwitchStatement:
				return visitSwitchStatement(ctx, <ts.SwitchStatement>node);
			case ts.SyntaxKind.LabeledStatement:
				return visitLabeledStatement(ctx, <ts.LabeledStatement>node);
			case ts.SyntaxKind.ThrowStatement:
				return visitThrowStatement(ctx, <ts.ThrowStatement>node);
			case ts.SyntaxKind.TryStatement:
				return visitTryStatement(ctx, <ts.TryStatement>node);
			case ts.SyntaxKind.ReturnStatement:
				return visitReturnStatement(ctx, <ts.ReturnStatement>node);

			// ignoring
			// case ts.SyntaxKind.ImportDeclaration:
			// case ts.SyntaxKind.BreakStatement:
			// case ts.SyntaxKind.ContinueStatement:
			// case ts.SyntaxKind.WithStatement:
			// case ts.SyntaxKind.NotEmittedStatement:
			// case ts.SyntaxKind.EmptyStatement:
			// case ts.SyntaxKind.DebuggerStatement:
		}
	}
	function visitSourceFile(ctx: Context, node: ts.SourceFile): void {
		ctx.pushScope((<NodeWithLocals><unknown>node).locals)
		for (const statement of node.statements) {
			visitStatement(ctx, statement, ts.SyntaxKind.SourceFile);
		}
		ctx.popScope();
	}

	function findBindingElementParentDeclaration(element: ts.BindingElement): ts.VariableDeclaration | ts.ParameterDeclaration {
		while (true) {
			if (element.parent.parent.kind === ts.SyntaxKind.BindingElement) {
				element = element.parent.parent;
			} else {
				return element.parent.parent;
			}
		}
	}

	function isLocalDeclaration(decl: ts.Declaration, sourceFile: ts.SourceFile): boolean {
		if (ts.isBindingElement(decl)) {
			decl = findBindingElementParentDeclaration(decl);
		}
		if (ts.isVariableDeclaration(decl) || ts.isBindingElement(decl)) {
			return (!ts.isSourceFile(decl.parent.parent.parent) || ts.isCatchClause(decl.parent)) && decl.getSourceFile() === sourceFile;
		} else if (ts.isFunctionDeclaration(decl)) {
			return !ts.isSourceFile(decl.parent) && decl.getSourceFile() === sourceFile;
		}
		return false;
	}

	function typeHasConstructSignatures(type: ts.Type): boolean {
		if (type.isUnion()) {
			for (const t of type.types) {
				if (t.getConstructSignatures().length > 0) {
					return true;
				}
			}
			return false;
		}
		return (type.getConstructSignatures().length > 0);
	}

	function typeHasCallSignatures(type: ts.Type): boolean {
		if (type.isUnion()) {
			for (const t of type.types) {
				if (t.getCallSignatures().length > 0) {
					return true;
				}
			}
			return false;
		}
		return (type.getCallSignatures().length > 0);
	}

	function typeHasProperties(type: ts.Type): boolean {
		if (type.isUnion()) {
			for (const t of type.types) {
				if (t.getProperties().length > 0) {
					return true;
				}
			}
			return false;
		}
		return (type.getProperties().length > 0);
	}

	function getEncodedSemanticClassifications(languageService: ts.LanguageService, fileName: string, _logger?: ts.server.Logger) {
		const program = languageService.getProgram();
		if (!program) {
			return [];
		}
		const sourceFile = program.getSourceFile(fileName);
		if (!sourceFile) {
			return [];
		}
		const context = new Context(program, sourceFile);
		visitSourceFile(context, sourceFile);
		return context.result;
	}

	function compareTokens(sourceFile: ts.SourceFile, actualTokens: number[], oracleTokens: number[]): string[] {
		const result: string[] = [];
		for (let i = 0, len = Math.max(oracleTokens.length / 3, actualTokens.length / 3); i < len; i++) {
			const offset = 3 * i;
			const oracleOffset = (offset < oracleTokens.length ? oracleTokens[offset] : -1);
			const oracleLength = (offset < oracleTokens.length ? oracleTokens[offset + 1] : -1);
			const oracleType = (offset < oracleTokens.length ? oracleTokens[offset + 2] : -1);
			const actualOffset = (offset < actualTokens.length ? actualTokens[offset] : -1);
			const actualLength = (offset < actualTokens.length ? actualTokens[offset + 1] : -1);
			const actualType = (offset < actualTokens.length ? actualTokens[offset + 2] : -1);
			if (oracleOffset !== actualOffset || oracleLength !== actualLength || oracleType !== actualType) {
				let debugStr = ``;
				debugStr += `THEIRS: ${printSemanticToken(sourceFile, oracleOffset, oracleLength, oracleType)}\n`;
				debugStr += `OURS: ${printSemanticToken(sourceFile, actualOffset, actualLength, actualType)}\n`;
				// const oracleNode = findNodeEncompassingRange(sourceFile, oracleOffset, oracleLength);
				// const actualNode = findNodeEncompassingRange(sourceFile, actualOffset, actualLength);
				result.push(debugStr);
				if (oracleOffset !== actualOffset) {
					break;
				}
			}
		}
		return result;

		function printSemanticToken(sourceFile: ts.SourceFile, offset: number, length: number, type: number): string {
			if (offset === -1) {
				return `<none>`;
			}
			const node = findNodeEncompassingRange(sourceFile, offset, length);
			let chain: string[] = [];
			let tmp: ts.Node | undefined = node;
			while (tmp) {
				chain.push((<any>tmp).__debugKind);
				tmp = tmp.parent;
			}
			chain.reverse();
			const tokenType = (type >>> TokenEncodingConsts.typeOffset) - 1;
			const tokenModifiers = (type & TokenEncodingConsts.modifierMask);
			const lineChar = sourceFile.getLineAndCharacterOfPosition(node.pos + node.getLeadingTriviaWidth());

			return `<<<${node.getText()}>>> @ ${lineChar.line + 1}:${lineChar.character} --- ${chain.join('>')}    ::::    ${tokenTypeToString(tokenType)} - [${tokenModifiersToString(tokenModifiers)}]`;
		}
		function findNodeEncompassingRange(sourceFile: ts.SourceFile, offset: number, length: number): ts.Node {
			const searchStart = offset;
			const searchEnd = offset + length;
			let current: ts.Node = sourceFile;
			outer: while (true) {
				// find the child that contains 'position'
				for (const child of current.getChildren(sourceFile)) {
					const childStart = child.getStart(sourceFile, /* includeJsDocComment */ true);
					const childEnd = child.getEnd();
					if (childStart <= searchStart && searchEnd <= childEnd) {
						// Good child
						current = child;
						continue outer;
					}
				}
				return current;
			}
		}
		function tokenTypeToString(tokenType: TokenType): string {
			switch (tokenType) {
				case TokenType.class: return 'class';
				case TokenType.enum: return 'enum';
				case TokenType.interface: return 'interface';
				case TokenType.namespace: return 'namespace';
				case TokenType.typeParameter: return 'typeParameter';
				case TokenType.type: return 'type';
				case TokenType.parameter: return 'parameter';
				case TokenType.variable: return 'variable';
				case TokenType.enumMember: return 'enumMember';
				case TokenType.property: return 'property';
				case TokenType.function: return 'function';
				case TokenType.member: return 'member';
			}
			return '<unknown>';
		}
		function tokenModifierToString(tokenModifier: TokenModifier): string {
			switch (tokenModifier) {
				case TokenModifier.declaration: return 'declaration';
				case TokenModifier.static: return 'static';
				case TokenModifier.async: return 'async';
				case TokenModifier.readonly: return 'readonly';
				case TokenModifier.defaultLibrary: return 'defaultLibrary';
				case TokenModifier.local: return 'local';
			}
			return '<unkown>';
		}
		function tokenModifiersToString(tokenModifiers: number): string[] {
			const result: string[] = [];
			for (let i = 0; i < TokenModifier._; i++) {
				const mask = ((1 << i) >>> 0);
				if (tokenModifiers & mask) {
					result.push(tokenModifierToString(i));
				}
			}
			return result;
		}
	}
	return {
		getEncodedSemanticClassifications,
		compareTokens
	}
}
