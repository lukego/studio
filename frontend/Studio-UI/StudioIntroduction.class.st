Class {
	#name : #StudioIntroduction,
	#superclass : #Object,
	#category : #'Studio-UI'
}

{ #category : #'world menu' }
StudioIntroduction class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #StudioIntroduction)
		order: 200.0;
		parent: #Studio;
		label: 'Open "Welcome to Studio" top-level starting point';
		action: [ self new open ].
]

{ #category : #api }
StudioIntroduction class >> open [
	^ self new open
]

{ #category : #api }
StudioIntroduction >> open [
	^GtInspector openOnPlayBook:
		(Gt2Document forFile: Studio dir / 'doc' / 'Studio.pillar').                                                    

]
