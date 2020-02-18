/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

import 'mocha';
import * as assert from 'assert';
import * as path from 'path';

import * as ts from 'typescript/lib/tsserverlibrary';
import initPlugin from '../index';
import { TokenType, TokenModifier, TokenEncodingConsts } from '../constants';

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
tokenTypes[TokenType.enumMember] = 'enumMember';
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

function assertTokens(mainFileName: string, files: { [name: string]: string } = {}, expected: ExpectedToken[], span?: ts.TextSpan): void {
    const sourceRoot = '/root/sources/';
    const mainFilePath = sourceRoot + mainFileName;
    const reactPath = require.resolve('@types/react/index.d.ts');

    function findFilePath(moduleName: string) {
        for (let fileName in files) {
            if (fileName.startsWith(moduleName + '.')) {
                return sourceRoot + fileName;
            }
        }
        return undefined;
    }

    let compilerOptions: ts.CompilerOptions = {
        allowNonTsExtensions: true, allowJs: true, lib: ['lib.es6.d.ts'],
        target: ts.ScriptTarget.Latest, moduleResolution: ts.ModuleResolutionKind.NodeJs,
        jsx: ts.JsxEmit.React, allowSyntheticDefaultImports: true
    };
    const host: ts.LanguageServiceHost = {
        getCompilationSettings: () => compilerOptions,
        getScriptFileNames: () => [...Object.keys(files).map(k => sourceRoot + k)],
        getScriptKind: (_fileName) => {
            const ext = path.extname(_fileName);
            switch (ext) {
                case '.js': return ts.ScriptKind.JS;
                case '.tsx': return ts.ScriptKind.TSX;
                case '.jsx': return ts.ScriptKind.JSX;
                default: return ts.ScriptKind.TS;
            }
        },
        getScriptVersion: (_fileName: string) => '1',
        getScriptSnapshot: (fileName: string) => {
            let text: string;
            if (fileName.startsWith(sourceRoot)) {
                text = files[fileName.substr(sourceRoot.length)] || '';
            } else {
                text = ts.sys.readFile(fileName) || '';
            }
            return {
                getText: (start, end) => text.substring(start, end),
                getLength: () => text.length,
                getChangeRange: () => undefined
            };
        },
        getCurrentDirectory: () => __dirname,
        // getDirectories: ts.sys.getDirectories,
        // directoryExists: ts.sys.directoryExists,
        // fileExists: ts.sys.fileExists,
        // readFile: ts.sys.readFile,

        resolveModuleNames: (moduleNames: string[], containingFile: string): (ts.ResolvedModule | undefined)[] => {
            const resolvedModules: (ts.ResolvedModule | undefined)[] = [];
            for (const moduleName of moduleNames) {
                if (moduleName.startsWith('./')) {
                    let resolvedFileName = findFilePath(moduleName.substring(2));
                    if (!resolvedFileName) {
                        resolvedFileName = path.join(path.dirname(containingFile), moduleName + '.ts');
                    }
                    resolvedModules.push({ resolvedFileName });
                } else if (moduleName === 'react') {
                    resolvedModules.push({ resolvedFileName: reactPath })
                } else {
                    resolvedModules.push(undefined);
                }
            }
            return resolvedModules;
        },
        getDefaultLibFileName: (options) => ts.getDefaultLibFilePath(options)
    };

    let languageService = ts.createLanguageService(host);


    languageService = initPlugin({ typescript: ts }).decorate(languageService);

    const mainContent = files[mainFileName];

    if (!span) {
        span = { start: 0, length: mainContent.length };
    }

    const diagnostics = languageService.getSemanticDiagnostics(mainFilePath);
    for (let d of diagnostics) {
        console.log(d.messageText);
    }
    for (let k in files) {
        const diagnostics = languageService.getSemanticDiagnostics(sourceRoot + k);
        for (let d of diagnostics) {
            console.log(d.messageText);
        }
    }

    const result = languageService.getEncodedSemanticClassifications(mainFilePath, span);

    const sourceFile = languageService.getProgram()?.getSourceFile(mainFilePath)!;
    let actualRanges = [];
    let snippet = '';
    let i = 0;
    while (i < result.spans.length) {
        const start = result.spans[i++], len = result.spans[i++], classification = result.spans[i++];
        const lineAndChar = sourceFile.getLineAndCharacterOfPosition(start)!;
        const typeIdx = getTokenTypeFromClassification(classification) || 0;
        const modSet = getTokenModifierFromClassification(classification);

        const tokenClassifiction = [tokenTypes[typeIdx], ...tokenModifiers.filter((_, i) => modSet & 1 << i)].join('.');
        actualRanges.push(t(lineAndChar.line, lineAndChar.character, len, tokenClassifiction));
        snippet += `t(${lineAndChar.line}, ${lineAndChar.character}, ${len}, '${tokenClassifiction}'), `;
    }
    assert.deepEqual(actualRanges, expected, snippet);

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
        assertTokens('main.ts', { 'main.ts': input }, [
            t(0, 6, 1, 'variable.declaration'), t(0, 13, 2, 'variable.declaration'), t(0, 19, 1, 'variable'),
            t(2, 15, 1, 'variable.declaration.readonly'), t(2, 20, 2, 'variable'), t(2, 26, 1, 'variable'), t(2, 30, 1, 'variable.readonly'),
            t(3, 11, 1, 'variable.declaration'),
            t(4, 10, 2, 'variable')
        ]);
    });

    test('Functions', () => {
        const input = [
			/*0*/'function foo(p1) {',
			/*1*/'  return foo(Math.abs(p1))',
			/*2*/'}',
            /*3*/'`/${window.location}`.split("/").forEach(s => foo(s));',
            /*4*/'const match = (s: string) => s.length;',
            /*4*/'const other = match',
        ].join('\n');
        assertTokens('main.ts', { 'main.ts': input }, [
            t(0, 9, 3, 'function.declaration'), t(0, 13, 2, 'parameter.declaration'),
            t(1, 9, 3, 'function'), t(1, 13, 4, 'interface'), t(1, 18, 3, 'member'), t(1, 22, 2, 'parameter'),
            t(3, 4, 6, 'variable'), t(3, 11, 8, 'property'), t(3, 22, 5, 'member'), t(3, 33, 7, 'member'), t(3, 41, 1, 'parameter.declaration'), t(3, 46, 3, 'function'), t(3, 50, 1, 'parameter'),
            t(4, 6, 5, 'function.declaration.readonly'), t(4, 15, 1, 'parameter.declaration'), t(4, 29, 1, 'parameter'), t(4, 31, 6, 'property.readonly'),
            t(5, 6, 5, 'function.declaration.readonly'), t(5, 14, 5, 'function.readonly')
        ]);
    });

    test('Members', () => {
        const input = [
			/*0*/'class A {',
			/*1*/'  static x = 9;',
			/*2*/'  f = 9;',
			/*3*/'  async m() { return A.x + await this.m(); };',
			/*4*/'  get s() { return this.f; ',
			/*5*/'  static t() { return new A().f; };',
			/*6*/'  constructor() {}',
			/*7*/'}',
        ].join('\n');
        assertTokens('main.ts', { 'main.ts': input }, [
            t(0, 6, 1, 'class.declaration'), t(1, 9, 1, 'property.declaration.static'),
            t(2, 2, 1, 'property.declaration'),
            t(3, 8, 1, 'member.declaration.async'), t(3, 21, 1, 'class'), t(3, 23, 1, 'property.static'), t(3, 38, 1, 'member.async'),
            t(4, 6, 1, 'property.declaration'), t(4, 24, 1, 'property'),
            t(5, 9, 1, 'member.declaration.static'), t(5, 26, 1, 'class'), t(5, 30, 1, 'property')
        ]);
    });

    test('Object Properties', () => {
        const input = [
			/*0*/'let x = 1, y = 1;',
			/*1*/'const a1 = { e: 1 };',
			/*2*/'var a2 = { x };'
        ].join('\n');
        assertTokens('main.ts', { 'main.ts': input }, [
            t(0, 4, 1, 'variable.declaration'), t(0, 11, 1, 'variable.declaration'),
            t(1, 6, 2, 'variable.declaration.readonly'), t(1, 13, 1, 'property.declaration'),
            t(2, 4, 2, 'variable.declaration')
        ]);
    });

    test('Interfaces', () => {
        const input = [
			/*0*/'interface Position { x: number, y: number };',
			/*1*/'const p = { x: 1, y: 2 } as Position;',
			/*2*/'const foo = (o: Position) => o.x + o.y;',
        ].join('\n');
        assertTokens('main.ts', { 'main.ts': input }, [
            t(0, 10, 8, 'interface.declaration'), t(0, 21, 1, 'property.declaration'), t(0, 32, 1, 'property.declaration'),
            t(1, 6, 1, 'variable.declaration.readonly'), t(1, 12, 1, 'property.declaration'), t(1, 18, 1, 'property.declaration'), t(1, 28, 8, 'interface'),
            t(2, 6, 3, 'function.declaration.readonly'), t(2, 13, 1, 'parameter.declaration'), t(2, 16, 8, 'interface'), t(2, 29, 1, 'parameter'), t(2, 31, 1, 'property'), t(2, 35, 1, 'parameter'), t(2, 37, 1, 'property')
        ]);
    });

    test('Enums', () => {
        const input = [
			/*0*/'export enum TextDocumentSaveReason {',
			/*1*/'  Manual = 1',
			/*2*/'}',
        ].join('\n');
        assertTokens('main.ts', { 'main.ts': input }, [
            t(0, 12, 22, 'enum.declaration'),
            t(1, 2, 6, 'enumMember.declaration.readonly')
        ]);
    });


    test('Readonly', () => {
        const input = [
			/*0*/'const f = 9;',
			/*1*/'class A { static readonly t = 9; static url: URL; }',
			/*2*/'const enum E { A = 9, B = A + 1 }',
			/*3*/'console.log(f + A.t + A.url.origin + E.A);',
        ].join('\n');
        assertTokens('main.ts', { 'main.ts': input }, [
            t(0, 6, 1, 'variable.declaration.readonly'),
            t(1, 6, 1, 'class.declaration'), t(1, 26, 1, 'property.declaration.static.readonly'), t(1, 40, 3, 'property.declaration.static'), t(1, 45, 3, 'interface'),
            t(2, 11, 1, 'enum.declaration'), t(2, 15, 1, 'enumMember.declaration.readonly'), t(2, 22, 1, 'enumMember.declaration.readonly'), t(2, 26, 1, 'enumMember.readonly'),
            t(3, 0, 7, 'variable'), t(3, 8, 3, 'member'), t(3, 12, 1, 'variable.readonly'), t(3, 16, 1, 'class'), t(3, 18, 1, 'property.static.readonly'), t(3, 22, 1, 'class'), t(3, 24, 3, 'property.static'), t(3, 28, 6, 'property.readonly'), t(3, 37, 1, 'enum'), t(3, 39, 1, 'enumMember.readonly')
        ]);
    });


    test('Type aliases and type parameters', () => {
        const input = [
			/*0*/'type MyMap = Map<string, number>;',
			/*1*/'function f<T extends MyMap>(t: T | number) : T { ',
			/*2*/'  return <T> <unknown> new Map<string, MyMap>();',
			/*3*/'}',
        ].join('\n');
        assertTokens('main.ts', { 'main.ts': input }, [
            t(0, 5, 5, 'type.declaration'), t(0, 13, 3, 'interface'),
            t(1, 9, 1, 'function.declaration'), t(1, 11, 1, 'typeParameter.declaration'), t(1, 21, 5, 'type'), t(1, 28, 1, 'parameter.declaration'), t(1, 31, 1, 'typeParameter'), t(1, 45, 1, 'typeParameter'),
            t(2, 10, 1, 'typeParameter'), t(2, 27, 3, 'interface'), t(2, 39, 5, 'type')


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
        assertTokens('main.ts', { 'main.ts': input, 'other.ts': other }, [
            t(0, 9, 1, 'class'), t(0, 12, 1, 'interface'), t(0, 15, 1, 'function'), t(0, 18, 1, 'variable.readonly'),
            t(1, 0, 1, 'class'), t(1, 2, 1, 'property.static'), t(1, 10, 1, 'function'), t(1, 16, 1, 'variable.readonly')
        ]);
    });

    test('JSX', () => {
        const input = [
            /*0*/'import React from "react";',
            /*1*/'function () {',
            /*2*/'  return (<div className="App">{React.version}</div>);',
            /*3*/'}',
        ].join('\n');
        assertTokens('main.tsx', { 'main.tsx': input }, [
            t(0, 7, 5, 'namespace'),
            t(2, 32, 5, 'namespace'), t(2, 38, 7, 'variable.readonly')
        ]);
    });


    test('JSX2', () => {
        const input = [
            /*0*/'const MyComponent = (props) => <div></div>',
            /*1*/'const ItemPrice = (props) => {',
            /*2*/'  return <MyComponent { ...props } />;',
            /*3*/'}',
        ].join('\n');
        assertTokens('main.tsx', { 'main.tsx': input }, [
            t(0, 6, 11, 'function.declaration.readonly'), t(0, 21, 5, 'parameter.declaration'),
            t(1, 6, 9, 'function.declaration.readonly'), t(1, 19, 5, 'parameter.declaration')
        ]);
    });
});