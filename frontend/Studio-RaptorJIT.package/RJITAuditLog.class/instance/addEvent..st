initialization
addEvent: dict
	| event |
	event := self makeEventFrom: dict.
	event number: events size + 1.
	"event informUser."
	events add: event.
