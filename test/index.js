/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

const Mocha = require('mocha');
const glob = require('glob');

const options = {
	ui: 'tdd',
	useColors: (!process.env.BUILD_ARTIFACTSTAGINGDIRECTORY && process.platform !== 'win32'),
    timeout: 60000
};

const mocha = new Mocha(options);
glob.sync(__dirname + '/../lib/test/**/*.test.js').forEach(file => mocha.addFile(file));
mocha.run(failures => process.exit(failures ? -1 : 0));
