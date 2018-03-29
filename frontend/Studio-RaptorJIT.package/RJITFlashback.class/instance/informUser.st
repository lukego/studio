ui
informUser
	UIManager default inform:
		('Loaded object #{1}: {2} ({3} bytes) at 0x{4}' format: 
			{ time. hint. data size. address radix: 16. })