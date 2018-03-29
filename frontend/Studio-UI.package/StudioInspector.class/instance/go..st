scripting opening
go: code
	[ :job |
		| product |
		job title: 'Studio backend is building data to inspect'.
		product := [ NixBuild onCode: code ] on: NixError do: [ :error |
			| dialog |
			dialog := NixErrorWindow openWithMessage: error messageText.
			dialog window
				position: self window position + (self window extent * 0.05);
				extent: self window extent * 0.9.
			^ nil ].
		job title: 'Studio frontend is analyzing data from the backend'.
		^ RJITProcess new fromPath: product firstOutput.
	] asJob run.
