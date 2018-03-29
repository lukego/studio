initializing
addMemory: dict
   flashback := RJITFlashback new
		auditLog: self;
      previous: flashback;
      address: (dict at: 'address');
      data: (dict at: 'data') asByteArray;
      hint: (dict at: 'hint');
		setup.
	"flashback informUser."
	"Detect special memory."
	flashback hint = 'lj_ir_mode' ifTrue: [ irModes := flashback asIrModes ]
	