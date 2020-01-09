A [TypeScript plugin](https://github.com/Microsoft/TypeScript/wiki/Writing-a-Language-Service-Plugin) that replaces `getEncodedSemanticClassifications` and `getEncodedSyntacticClassifications` to provide more classifications.

New clssifications are returned. Each classification is a combination of a TokenType with any number of TokenModifiers.

```ts
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
```