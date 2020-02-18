/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

// typescript-vscode-sh-plugin encodes type and modifiers in the classification:
// TSClassification = (TokenType + 1) << 8 + TokenModifier

export const enum TokenType {
	class, enum, interface, namespace, typeParameter, type, parameter, variable, enumMember, property, function, member, _
}

export const enum TokenModifier {
	declaration, static, async, readonly, local, _
}

export const enum TokenEncodingConsts {
	typeOffset = 8,
	modifierMask = (1 << typeOffset) - 1
}

export declare const enum VersionRequirement {
	major = 3,
	minor = 7
}

