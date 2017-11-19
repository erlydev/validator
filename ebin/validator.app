{application, 'validator', [
	{description, "Data Validator"},
	{vsn, "0.1.0"},
	{modules, ['validator','validator_app','validator_sup']},
	{registered, [validator_sup]},
	{applications, [kernel,stdlib]},
	{mod, {validator_app, []}},
	{env, []}
]}.