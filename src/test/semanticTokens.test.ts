/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

import 'mocha';
import * as assert from 'assert';

import * as ts from 'typescript/lib/tsserverlibrary';
import initPlugin from '../index';
import { TokenType, TokenModifier, TokenEncodingConsts} from '../constants';

interface ExpectedToken {
	startLine: number;
	character: number;
	length: number;
	tokenClassifiction: string;
}

function t(startLine: number, character: number, length: number, tokenClassifiction: string): ExpectedToken {
	return { startLine, character, length, tokenClassifiction };
}


const tokenTypes: string[] = [];
tokenTypes[TokenType.class] = 'class';
tokenTypes[TokenType.enum] = 'enum';
tokenTypes[TokenType.interface] = 'interface';
tokenTypes[TokenType.namespace] = 'namespace';
tokenTypes[TokenType.typeParameter] = 'typeParameter';
tokenTypes[TokenType.type] = 'type';
tokenTypes[TokenType.parameter] = 'parameter';
tokenTypes[TokenType.variable] = 'variable';
tokenTypes[TokenType.property] = 'property';
tokenTypes[TokenType.function] = 'function';
tokenTypes[TokenType.member] = 'member';

const tokenModifiers: string[] = [];
tokenModifiers[TokenModifier.async] = 'async';
tokenModifiers[TokenModifier.declaration] = 'declaration';
tokenModifiers[TokenModifier.readonly] = 'readonly';
tokenModifiers[TokenModifier.static] = 'static';

function getTokenTypeFromClassification(tsClassification: number): number | undefined {
	if (tsClassification > TokenEncodingConsts.modifierMask) {
		return (tsClassification >> TokenEncodingConsts.typeOffset) - 1;
	}
	return undefined;
}

function getTokenModifierFromClassification(tsClassification: number) {
	return tsClassification & TokenEncodingConsts.modifierMask;
}

function assertTokens(mainContent: string, expected: ExpectedToken[], files: { [name:string]: string} = {}, span?: ts.TextSpan): void {
    const sourceRoot = '/root/sources/';
    const mainFileName = sourceRoot + 'main.ts';

    let compilerOptions: ts.CompilerOptions = { allowNonTsExtensions: true, allowJs: true, lib: ['lib.es6.d.ts'], target: ts.ScriptTarget.Latest, moduleResolution: ts.ModuleResolutionKind.NodeJs };
	const host: ts.LanguageServiceHost = {
		getCompilationSettings: () => compilerOptions,
		getScriptFileNames: () => [mainFileName, ...Object.keys(files).map(k => sourceRoot + k)],
		getScriptKind: (_fileName) => ts.ScriptKind.TS,
		getScriptVersion: (_fileName: string) => '1',
		getScriptSnapshot: (fileName: string) => {
            let text: string;
            if (fileName.startsWith(sourceRoot)) {
                if (fileName === mainFileName) {
                    text = mainContent;
                } else {
                    text = files[fileName.substr(sourceRoot.length)] || '';
                }
            } else {
                text = ts.sys.readFile(fileName) || '';
            }
			return {
				getText: (start, end) => text.substring(start, end),
				getLength: () => text.length,
				getChangeRange: () => undefined
			};
		},
		getCurrentDirectory: () => '',
		getDefaultLibFileName: (options) => ts.getDefaultLibFilePath(options)
	};
    let languageService = ts.createLanguageService(host);
    languageService = initPlugin({ typescript: ts }).decorate(languageService);

    if (!span) {
        span = { start: 0, length: mainContent.length };
    }

    const diagnostics = languageService.getSemanticDiagnostics(mainFileName);
    for (let d of diagnostics) {
        console.log(d.messageText);
    }
    for (let k in files) {
        const diagnostics = languageService.getSemanticDiagnostics(sourceRoot + k);
        for (let d of diagnostics) {
            console.log(d.messageText);
        }
    }
    



    const result = languageService.getEncodedSemanticClassifications(mainFileName, span);

    const sourceFile = languageService.getProgram()?.getSourceFile(mainFileName)!;
    let actualRanges = [];
	let i = 0;
	while (i < result.spans.length) {
        const start = result.spans[i++], len = result.spans[i++], classification = result.spans[i++];
        const lineAndChar = sourceFile.getLineAndCharacterOfPosition(start)!;
        const typeIdx = getTokenTypeFromClassification(classification) || 0;
        const modSet = getTokenModifierFromClassification(classification);

		const tokenClassifiction = [tokenTypes[typeIdx], ...tokenModifiers.filter((_, i) => modSet & 1 << i)].join('.');
		actualRanges.push(t(lineAndChar.line, lineAndChar.character, len, tokenClassifiction));
    }
    assert.deepEqual(actualRanges, expected);

}

suite('HTML Semantic Tokens', () => {

	test('Variables', () => {
		const input = [
			/*0*/'  var x = 9, y1 = [x];',
			/*1*/'  try {',
			/*2*/'    for (const s of y1) { x = s }',
			/*3*/'  } catch (e) {',
			/*4*/'    throw y1;',
			/*5*/'  }',
		].join('\n');
		assertTokens(input, [
			t(0, 6, 1, 'variable.declaration'), t(0, 13, 2, 'variable.declaration'), t(0, 19, 1, 'variable'),
			t(2, 15, 1, 'variable.declaration.readonly'), t(2, 20, 2, 'variable'), t(2, 26, 1, 'variable'), t(2, 30, 1, 'variable.readonly'),
			t(3, 11, 1, 'variable.declaration'),
			t(4, 10, 2, 'variable')
		]);
    });

    test('Import', () => {
		const input = [
            /*0*/'import { A, I, f, c } from "./other"',
            /*1*/'A.f = 8 + f() + c;',
        ].join('\n');
        const other = [
            /*0*/'export class A { public static f = 9; }',
            /*1*/'export interface I { }',
            /*2*/'export function f() : number { return 1; }',
            /*3*/'export const c = 9;',
		].join('\n');
		assertTokens(input, [
            t(0, 9, 1, 'class'), t(0, 12, 1, 'interface'), t(0, 15, 1, 'function'), t(0, 18, 1, 'variable.readonly'),
            t(1, 0, 1, 'class'), t(1, 2, 1, 'property.static'), t(1, 10, 1, 'function'), t(1, 16, 1, 'variable.readonly')
		], { 'other.ts': other });
    });
});