initialization
loadFromFileNamed: fileName
	| log mp stream count |
	[ :job |
		job title: 'Reading audit.log into memory'.
		events := OrderedCollection new.
		"Read into memory."
		log := fileName asFileReference binaryReadStream contentsOfEntireFile.
		stream := ReadStream on: log.
		mp := MpDecoder on: stream.
		count := 1.
		[  mp atEnd ] whileFalse: [
			job title: 'Reading audit.log record #', count asString.
			self addRecord: mp next.
			count := count + 1.
			job progress: stream position / log size. ]
	] asJob run.
	UIManager default inform: 'Loaded ', events size asString, ' RaptorJIT events (auditlog)'.