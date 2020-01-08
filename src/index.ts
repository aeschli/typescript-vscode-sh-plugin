
import * as ts_module from 'typescript/lib/tsserverlibrary';

export = function init({ typescript }: { typescript: typeof ts_module }) {
    
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

    intercept.getEncodedSemanticClassifications = (...args) => {
        return getEncodedSemanticClassifications(...args);
    };


    return new Proxy(languageService, {
        get: (target: any, property: keyof ts.LanguageService) => {
            return intercept[property] || target[property];
        },
    });
}

function getEncodedSemanticClassifications(filename: string, span: ts_module.TextSpan) : ts_module.Classifications {
    return {
        spans: [],
        endOfLineState: ts_module.EndOfLineState.None
    }
}