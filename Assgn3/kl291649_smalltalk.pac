| package |
package := Package name: 'kl291649_smalltalk'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #PointColl;
	add: #Torus;
	add: #TPoint;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'kl291649 - smalltalk - gotowe rozwiazanie\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #Torus
	instanceVariableNames: 's dict'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #TPoint
	instanceVariableNames: 'val coordinates torus'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Collection subclass: #PointColl
	instanceVariableNames: 'oCollection'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Torus guid: (GUID fromString: '{1F2B8FD0-613A-4B19-B522-F3BBFB0373CB}')!
Torus comment: ''!
!Torus categoriesForClass!Unclassified! !
!Torus methodsFor!

addKey: key andVal: point
        dict = self getDict.
	dict at: key put: point.!

dajPunkt: wspolrzedne1
	|odp point i x   wspolrzedne| 
	i := 1. 
	wspolrzedne := wspolrzedne1 copy.
	[i <= (wspolrzedne size)] whileTrue:
		[x := wspolrzedne at: i.
		wspolrzedne at:i put: ((x) \\ ((self getS at: i) ) ).
		 i := i + 1
		]. 
	dict := self getDict.
	(dict includesKey: wspolrzedne) ifTrue: "gdy jest prosba o istniejacy punkt torusa to go zwroc"
		[odp := dict at: wspolrzedne].
	(dict includesKey: wspolrzedne) ifFalse: "gdy jest prosba o nieistniejacy punkt torusa to go stworz i zwroc"
		[point := TPoint new. 
		point setSize: wspolrzedne setTorus: self.
		dict at: wspolrzedne put: point .
		odp := point].
	^odp!

getDict
	"Private - Answer the value of the receiver's 'dict' instance variable."

	^dict!

getS
	"Private - Answer the value of the receiver's 's' instance variable."

	^s!

initialize
	s := OrderedCollection new.
	dict := Dictionary new. !

setDict: anObject
	"Private - Set the value of the receiver's 'dict' instance variable to the argument."

	dict := anObject!

setS: anObject
	"Private - Set the value of the receiver's 's' instance variable to the argument."

	s := anObject! !
!Torus categoriesFor: #addKey:andVal:!public! !
!Torus categoriesFor: #dajPunkt:!public! !
!Torus categoriesFor: #getDict!accessing!private! !
!Torus categoriesFor: #getS!accessing!private! !
!Torus categoriesFor: #initialize!accessing!private! !
!Torus categoriesFor: #setDict:!accessing!private! !
!Torus categoriesFor: #setS:!accessing!private! !

!Torus class methodsFor!

shape: x 
	|torus point rozmiar i tab dict|
	torus := self new.
	rozmiar := x size.   
	
	
	tab := Array new: rozmiar.
	i := 1. 
	[i <= rozmiar] whileTrue:
		[tab at: i put:0.
		 i := i + 1
		].

	point := TPoint new.
	point setSize: tab setTorus: torus.

	dict := Dictionary new. 
	torus setDict: dict.
	torus addKey: tab andVal: point.
	
  
	torus setS: x.
	"^torus getS first."
	"^point"
	^point! !
!Torus class categoriesFor: #shape:!public! !

TPoint guid: (GUID fromString: '{6A60A4C4-0D08-4699-A0FE-2B30E7F13373}')!
TPoint comment: ''!
!TPoint categoriesForClass!Unclassified! !
!TPoint methodsFor!

- index
	|wartoscind odp  tab |
	wartoscind := coordinates at: index.
	tab := coordinates copy. 
	tab at:index put: wartoscind - 1.
	odp := torus dajPunkt: tab. 
	^odp.!

% point
	| j odp i tab rozmiar result st nd sign ile current wynik| 
	st := point x.
	nd := point y.
	j := nd abs. " jest to numer indeksu w ksztalcie"
        sign := j / nd.

	rozmiar := torus getS at: j." rozmiar danego indeksu"
	result := OrderedCollection  new.
	ile := 1.
	i := coordinates at: j.
	tab := coordinates copy.
	tab at: j put: i.
	odp := torus dajPunkt: tab.
	result add: odp.
	tab := coordinates copy. 
	i := i + sign.
	current := i.

	[i = (current - sign) | (ile = st)] whileFalse: "wyliczaj po kolei kolejne punkty"
			[tab at: j put: i.
			i := i + sign.
			ile := ile + 1.
			i := i \\ rozmiar.
			odp := torus dajPunkt: tab.
			result add: odp].
	wynik := PointColl zwroc: result.

	^wynik!

@ kolekcja
	|  odp i x  tab |
	i := 1.
	tab := kolekcja copy.
	[i <= (kolekcja size)] whileTrue:
		[x := coordinates at: i.
		tab at:i put: (x + (kolekcja at: i) ).
		 i := i + 1
		]. 
 
	odp := torus dajPunkt: tab. 
	^odp.!

| index 
	| j odp i tab rozmiar result sign current wynik|
	j := index abs.
        sign := j / index.
	rozmiar := torus getS at: j.
	result := OrderedCollection new.
	i := coordinates  at: j.
 
	result add: self.
	tab := coordinates copy.
	i := i + sign.
	current := i.

	[i = (current- sign)] whileFalse:   "wyliczaj po kolei kolejne punkty"
			[tab at: j put: i. 
			i := i + sign.
			i := i \\ rozmiar.
			odp := torus dajPunkt: tab.
			result add: odp].

	wynik := PointColl zwroc: result.
	^wynik!

+ index
	|wartoscind odp  tab |
	wartoscind := coordinates at: index.
	tab := coordinates copy.
	tab at:index put: wartoscind + 1.
	odp := torus dajPunkt: tab. 
	^odp.!

intitiazlize
	val := nil.
	coordinates := nil.!

printOn: aStream
	^val printOn: aStream!

setSize: rozmiar 
	|i    tab|
	tab := Array new: rozmiar.
	i := 1. 
	[i <= rozmiar] whileTrue:
		[tab at: i put:0.
		 i := i + 1
		].
	coordinates := tab.
	^coordinates!

setSize: tab setTorus: tor 
	torus := tor. 
	coordinates := tab.
	^coordinates!

value
	^val!

value: elt
	^val := elt. ! !
!TPoint categoriesFor: #-!public! !
!TPoint categoriesFor: #%!public! !
!TPoint categoriesFor: #@!public! !
!TPoint categoriesFor: #|!public! !
!TPoint categoriesFor: #+!public! !
!TPoint categoriesFor: #intitiazlize!public! !
!TPoint categoriesFor: #printOn:!public! !
!TPoint categoriesFor: #setSize:!public! !
!TPoint categoriesFor: #setSize:setTorus:!public! !
!TPoint categoriesFor: #value!converting!private! !
!TPoint categoriesFor: #value:!private! !

PointColl guid: (GUID fromString: '{31829B11-0577-42A1-9F53-0EC1E0083F7A}')!
PointColl comment: ''!
!PointColl categoriesForClass!Unclassified! !
!PointColl methodsFor!

% num
	|wynik result elts |
	wynik := OrderedCollection  new.
	oCollection 
	do:[ :elt |
               elts := elt % num.
               wynik addAll: elts.
       ].
	result := PointColl zwroc: wynik.
	^result.!

_beginsString: aString
	^oCollection _beginsString: aString!

| num
	|wynik result elts   |
	wynik := OrderedCollection  new.
	oCollection 
	do:[ :elt |
               elts := elt | num.
               wynik addAll: elts.
       ].
	result := PointColl zwroc: wynik.
	^result.!

add: newElement
	"Include the <Object> argument, newElement, as one of the receiver's 
	elements. Answer newElement."

	^self shouldNotImplement!

addAll: newElements
	"Include all the elements of the <collection> argument, newElements, as the receiver's elements. 
	Answer newElements. 
	Note that this modifies the and answers the receiver, not a copy.
	newElements can actually be any object which understands #do:."

	newElements do: [:each | self add: each].
	^newElements!

allSatisfy: discriminator
	"Answer whether the <monadicValuable>, discriminator, evaluates to true for
	every element of the receiver.
	Implementation Note: The argument is evaluated for every element
	of the receiver iff it is satisfied for every element."

	^oCollection allSatisfy: discriminator!

anyOne
	"Answer an arbitrary element of the collection. Raise an error if the collection is empty."
	^oCollection anyOne!

anySatisfy: discriminator
	"Answer whether the <monadicValuable>, discriminator, evaluates to true for
	any element of the receiver.
	Implementation Note: The argument is evaluated for every element of the
	receiver iff it evaluates to false for every element."

	^oCollection anySatisfy: discriminator!

appendToStream: puttableStream
	"Private - Append the receiver's elements to the argument, puttableStream.
	Answer the receiver.
	Implementation note: Double dispatched from puttableStream>>nextPutAll:."

	^oCollection appendToStream: puttableStream!

approxSize
	"Private - Answer the approximate size of the receiver.
	Implementation Note: This does not have to be accurate, but it should be fast
	(i.e. don't calculate it by counting the receiver's elements).
	The default is to guess at 2. Subclasses which can give a more
	accurate size quickly will get better conversion performance."

	^oCollection approxSize!

asArray
	"Answer an <Array> whose elements are those of the receiver.
	The ordering is that of the #do: operation as implemented by the receiver,
	and the answer will normally be the same size as the receiver."

	^oCollection asArray!

asBag
	"Answer a <Bag> containing the same elements as the receiver
	Note: As Bags identify multiple occurrences with the equality relationship
	the enumerated elements of the result may not be identical to those of
	the receiver, although there will be the same number.
	Implementation Note: It is an error if the receiver contains nil elements,
	but this is not currently trapped."

	^oCollection asBag!

asByteArray
	"Answer a <ByteArray> whose elements are those of the receiver.
	The ordering is that of the #do: operation as implemented by the receiver,
	and the answer will normally be the same size as the receiver.
	This will fail if the receiver contains elements which are not Integers
	in the range 0..255."

	^oCollection asByteArray!

asIdentitySet
	"Answer a new <IdentitySet> whose elements are those stored in the receiver.
	Any duplicates are eliminated, so the result may be smaller than
	the receiver. Any nil elements of the receiver are also eliminated."

	^oCollection asIdentitySet!

asOrderedCollection
	"Answer an <OrderedCollection> whose elements are those of the receiver
	The ordering is that of the #do: operation as implemented by the receiver,
	and the answer will normally be the same size as the receiver.
	Implementation Note: Although we might be able to perform this operation 
	faster for collections which keep a tally of their size (or can otherwise 
	access it without calculation) we must be careful NOT to provide a default 
	implementation which will cause a double enumeration for collections which must
	count their elements to determine their size, which may be very slow."

	^oCollection asOrderedCollection!

aspectDisplayOn: aStream
	"Private - Append a single-line textual representatin of the receiver to the <puttableStream>
	argument in a form that a user viewing the receiver as the value of a published aspect would 
	like to see it. Typically we use #displayOn: but some classes of object can use alternate display 
	formats.In this case we don't want to include the contents.
	N.B. This is a development time only method that supports the PublishedAspectInspector."
	
	^oCollection aspectDisplayOn: aStream!

asSAFEARRAY
	"Answer the <SAFEARRAY> representation of the receiver.
	N.B. The result will be a single-dimensioed array of the variant representations
	of the receiver's elements, assuming that such a representation is possible."

	^oCollection asSAFEARRAY!

asSet
	"Answer a <Set> whose elements are those stored in the receiver.
	Any duplicates are eliminated, so the result may be smaller than
	the receiver. Any nil elements of the receiver are also eliminated,
	but this behaviour may differ between implementations, and so
	should not be relied upon in portable code."

	^oCollection asSet!

asSortedCollection
	"Answer a <SortedCollection> of the same size as the receiver
	whose elements are those of the receiver, with the order of the result
	being determined by the default sort block (see SortedCollection).
	Exceptions may occur if any of the elements of the receiver are not
	appropriate parameters for the default sort block."

	^oCollection asSortedCollection!

asSortedCollection: aDyadicValuable 
	"Answer a <SortedCollection> whose elements are those of the receiver, sorted according to 
	the <dynadicValuable> argument, sortBlock.
	Exceptions may occur if any of the elements of the receiver are not
	appropriate parameters for sortBlock.
	Note: The argument does not need to be a BlockClosure, it must simply
	understand the #value:value: message from the dyadic valuable protocol."

	^oCollection asSortedCollection: aDyadicValuable!

asSortedCollectionUsing: aSortAlgorithm 
	"Answer a <SortedCollection> whose elements are those of the receiver, sorted using the
	<SortAlgorithm> argument. Exceptions may occur if any of the elements of the receiver are
	not appropriate parameters for the aSortAlgorithm's sortBlock."

	^oCollection asSortedCollectionUsing: aSortAlgorithm!

asVariant
	"Answer the VARIANT representation of the receiver (a SAFEARRAY of VARIANT)."

	^oCollection asVariant!

collect: transformer
	"Evaluate the <monadicValuable> argument, transformer, for each of the 
	receiver's elements in the order defined by the receiver's implementation of #do:.
	Answer a new collection like the receiver (i.e. of the same #species but not 
	necessarily the exact same class - since the collection may hold a different objects,
	it may need to be of a different type, hence the #species /#copyLike mechanism) 
	containing the values returned by transformer on each evaluation."

	^oCollection collect: transformer!

copyEmpty
	"Answer an empty copy of the receiver (which must be of the exact same class
	not just the same #species), with enough capacity for the same number of elements.
	#copyEmpty can be useful when you wish to take a copy of a collection that
	preserves all of the collections attributes (including the capacity) apart from its 
	elements."

	^oCollection copyEmpty!

copyEmpty: anInteger
	"Private - Answer an empty copy of the receiver (which should be of the exact same class
	not just the same #species), with sufficient capacity for anInteger number of elements."

	"N.B. Must be reimplemented by subclasses that need to copy additional instance 
	variables (e.g. the sortBlock of SortedCollection)."

	^oCollection copyEmpty!

copyLike
	"Private - Answer an empty collection of the same species as the receiver with 
	sufficient capacity for anInteger number of elements."

	^oCollection copyLike!

copyLike: anInteger
	"Private - Answer an empty collection of the same species of the receiver with 
	sufficient capacity for anInteger number of elements."

	^oCollection copyLike: anInteger!

copySize
	"Private - Answer the size of empty copy to create when performing various
	copying/collecting transformations against the receiver. Can be overridden
	by subclasses for which #size is a slow operation."

	^oCollection copySize!

copyWithoutDuplicates
	"Answers a copy of the receiver that contains no duplicate objects. 
	Uses equality for comparison."

	^oCollection copyWithoutDuplicates!

countElements
	"Private - Count, and answer, the number of elements in the receiver.
	Implementation Note: Could be implemented more elegantly with #inject:into:, but
	this implementation is about twice as fast."

	^oCollection countElements!

createPointCollection: x
	oCollection := x!

detect: discriminator
	"Evaluate the <monadicValuable> argument, discriminator, for each of the receiver's elements
	in the order defined by the receiver's implementation of #do:.
	Answer the first element for which discriminator evaluates to true. If none evaluates to true,
	report an error."

	^oCollection detect: discriminator!

detect: discriminator ifNone: exceptionHandler
	"Evaluate the <monadicValuable> argument, discriminator, for each of the receiver's 
	elements.  Answer the first element (in the #do: order) for which discriminator evaluates 
	to true. If none evaluates to true answer the result of evaluating the <niladicValuable> 
	argument, exceptionHandler.
	Implementation Note: It is important for Shared subclasses that the exceptionHandler is 
	not evaluated inside the enumeration in case it is a niladic block containing an explicit 
	return (we try to avoid explicit returns from critical sections, as these require an unwind)."

	^oCollection detect: discriminator ifNone: exceptionHandler!

difference: comperand
	"Answer a <collection> like the receiver containing the Set theroetic 
	difference between the receiver and the <collection>, comperand. 
	i.e. Answer the set of all objects that are elements of the receiver 
	but not the argument."

	^oCollection difference: comperand!

do: operation
	"Evaluate the <monadicValuable> argument, operation, for each of the 
	receiver's elements.
	The exact visit ordering is unspecified at this level, but this message must be
	implemented by subclasses (as it provides much of the enumeration behaviour 
	of Collections), and those implementations are free to define any appropriate
	ordering."

	^oCollection do: operation!

do: operation separatedBy: separator 
	"Evaluate the <monadicValuable> argument, operation, for each of the 
	receiver's elements, interspersed with evaluations of the <niladicValuable>
	argument, separator. The separator is first evaluated after the first
	element, and is not evaluated after the last element (i.e. it is not evaluated
	at all if there are less than two elements)."

	^oCollection do: operation separatedBy: separator!

errorNotKeyed
	"Private - Generate an error to the effect that the receiver is not a keyed Collection"

	^oCollection errorNotKeyed!

growSize
	"Private - Answer the number of elements by which the receiver should grow, when growing!!
	(at least 2, in case the receiver is currently empty)"

	^oCollection growSize!

identityIncludes: anObject 
	"Answer whether the argument, anObject, is one of the receiver's elements."

	^oCollection identityIncludes: anObject!

includes: target
	"Answer whether the <Object> argument, target, is one of the elements of the receiver.
	Implementation Note: The default is to use equality for comparison."

	^oCollection includes: target!

inject: initialValue into: operation
	"Evaluate the <dyadicValuable> argument, operation, once for each element in the receiver, with
	that element as the second argument; and with the first argument as the value of the previous 
	evaluation, starting with the <Object> argument, initialValue. The operation must answer the value
	it wishes to have passed as its first argument the next time it is evaluated. The traversal is in the #do: 
	order. Answer the final value of the operation.
	This enumeration is particularly useful for performing summations and other statistical operations."

	^oCollection inject: initialValue into: operation!

intersection: comperand
	"Answer a new <collection>, like the receiver, that is the intersection of the 
	receiver and the <collection>, comperand, i.e. answer the set of all objects
	which are elements of both the receiver and the argument."

	^oCollection intersection: comperand!

isEmpty
	"Answer whether the receiver contains no elements."

	^oCollection isEmpty
!

maxPrint
	"Private - Answer the maximum number of characters to be printed onto a stream as 
	the string representation of the receiver."

	^oCollection maxPrint
!

newSelection
	"Private - Answer a new empty collection like the receiver to 
	contain a selection of the receiver's elements."

	^oCollection newSelection!

noDifference: aCollection 
	"Answer whether the receiver contains the same elements as the <collection> argument
	(i.e. the symmetric difference is empty)."

	^oCollection noDifference: aCollection!

notEmpty
	"Answer whether the receiver contains any elements."

	^oCollection notEmpty!

occurrencesOf: target
	"Answer the <integer> number of the receiver's elements which are 
	equal to the <Object> argument, target."

	^oCollection occurrencesOf: target!

printCyclicRefOn: aStream
	"Private - Append to the argument, aStream, a String whose characters describe
	a cyclic (or recursive) reference to the receiver. Used by some
	printOn: methods (e.g. see Collection) to prevent an infinite recursion."

	^oCollection printCyclicRefOn: aStream!

printOn: aStream
	"Print a string representation of self on aStream. This method suffices for 
	most collections, and is able to handle cyclic references."

	^oCollection printOn: aStream!

printPrefixOn: aStream
	"Private - Print a prefix string for the debug representation of the receiver on aStream."

	^oCollection printPrefixOn: aStream!

printSuffixOn: aStream
	"Private - Print a suffix string for the debug representation of the receiver on aStream."

	^oCollection printSuffixOn: aStream!

publishedAspects
    	"Answer a <LookupTable> of the <Aspect>s published by the receiver.
    	Implementation Note: By adding superclass aspects to the keyed aspect
    	table we get much better performance because it reduces the number 
    	of rehashes that are necessary due to resizing."
    
    	^oCollection publishedAspects!

publishedKeyedAspects
    	"Answers a LookupTable of the published aspects of the 
    	receiver's keyed contents."
    
    	"Implementation Note: Allow a little slop for published aspects
    	to be added."
    
    	^oCollection publishedKeyedAspects!

publishedKeyedAspectsBatchSize
    	"Private - Answers the number of keyed aspects to display in the PAI return at a time"
    
    	^oCollection publishedKeyedAspectsBatchSize!

rehash
	"Rehash the receiver to re-establish hash invariants, if any.
	The default is to do nothing, but this selector is present at this level, as any
	subclass could potentially be implemented as a hashed collection." !

reject: discriminator
	"Evaluate the <monadicValuable> argument, discriminator, for each of the receiver's elements.
	Answer a new <collection> like the receiver containing only those elements for which 
	the discriminator evaluates to false."

	^oCollection reject: discriminator!

remove: oldElement 
	"Remove the <Object> argument, oldElement, from the receiver's elements.
	Answer oldElement unless it is not an element of the receiver, in which 
	case, raise a suitable exception."

	^oCollection remove: oldElement!

remove: oldElement ifAbsent: exceptionHandler
	"Remove the <Object> argument, oldElement, from the receiver's elements.  
	If several of the elements are equal to anObject, only one is removed (precisely 
	which is determined by the the subclass).
	If no element is equal to oldElement, answer the result of evaluating the <niladicValuable>
	exceptionHandler. Otherwise, answer oldElement. 
	Must be implemented by subclasses which comply with <extensibleCollection>."

	^self shouldNotImplement!

removeAll: oldElements
	"Remove each element of the <collection>, oldElements, from the receiver, raising
	an Exception if any are not elements of the receiver. Answer oldElements."

	^oCollection removeAll: oldElements!

select: discriminator
	"Evaluate the <monadicValuable> argument, discriminator, for each of the receiver's elements.
	Answer a new <collection> like the receiver containing only those elements for which 
	the discriminator evaluates to true."

	^oCollection select: discriminator!

select: discriminator thenCollect: transformer
	"Answer a new <collection> like the receiver containing the values returned by 
	the <monadicValuable>, transformer, when evaluated for each of the receiver's 
	elements for which the <monadicValuable>, discriminator, evaluates to true."

	^oCollection select: discriminator thenCollect: transformer!

setOrderCollection: x
	oCollection := x!

size
	"Answer the <integer> number of elements in the receiver.
	Implementation Note: This implementation is rather inefficient, 
	and subclasses will probably want to override it."

	^oCollection size!

species
	^OrderedCollection!

storeOn: aStream 
	"Append to the <puttableStream> argument, target, an expression which when 
	evaluated will answer a collection similar to the receiver."

	^oCollection storeOn: aStream!

sunitbRemoveAll: aCollection

	^oCollection sunitbRemoveAll: aCollection!

symmetricDifference: comperand
	"Answer a <collection> like the receiver containing the symmetric
	difference of the receiver and the <collection>, comperand. 
	i.e. Answer the set of all objects that are elements of the receiver 
	or the argument, but not both."

	^oCollection symmetricDifference: comperand!

union: comperand
	"Answer a <Set> that is the union of the elements of receiver and 
	the <collection>, comperand. i.e. Answer the set of all objects that
	are elements of the receiver or the argument, or both."

	^oCollection union: comperand! !
!PointColl categoriesFor: #%!public! !
!PointColl categoriesFor: #_beginsString:!private! !
!PointColl categoriesFor: #|!public! !
!PointColl categoriesFor: #add:!public! !
!PointColl categoriesFor: #addAll:!public! !
!PointColl categoriesFor: #allSatisfy:!public! !
!PointColl categoriesFor: #anyOne!public! !
!PointColl categoriesFor: #anySatisfy:!public! !
!PointColl categoriesFor: #appendToStream:!private! !
!PointColl categoriesFor: #approxSize!private! !
!PointColl categoriesFor: #asArray!public! !
!PointColl categoriesFor: #asBag!public! !
!PointColl categoriesFor: #asByteArray!public! !
!PointColl categoriesFor: #asIdentitySet!public! !
!PointColl categoriesFor: #asOrderedCollection!public! !
!PointColl categoriesFor: #aspectDisplayOn:!private! !
!PointColl categoriesFor: #asSAFEARRAY!public! !
!PointColl categoriesFor: #asSet!public! !
!PointColl categoriesFor: #asSortedCollection!public! !
!PointColl categoriesFor: #asSortedCollection:!public! !
!PointColl categoriesFor: #asSortedCollectionUsing:!public! !
!PointColl categoriesFor: #asVariant!public! !
!PointColl categoriesFor: #collect:!public! !
!PointColl categoriesFor: #copyEmpty!public! !
!PointColl categoriesFor: #copyEmpty:!private! !
!PointColl categoriesFor: #copyLike!private! !
!PointColl categoriesFor: #copyLike:!private! !
!PointColl categoriesFor: #copySize!private! !
!PointColl categoriesFor: #copyWithoutDuplicates!public! !
!PointColl categoriesFor: #countElements!private! !
!PointColl categoriesFor: #createPointCollection:!public! !
!PointColl categoriesFor: #detect:!public! !
!PointColl categoriesFor: #detect:ifNone:!public! !
!PointColl categoriesFor: #difference:!public! !
!PointColl categoriesFor: #do:!public! !
!PointColl categoriesFor: #do:separatedBy:!public! !
!PointColl categoriesFor: #errorNotKeyed!private! !
!PointColl categoriesFor: #growSize!private! !
!PointColl categoriesFor: #identityIncludes:!public! !
!PointColl categoriesFor: #includes:!public! !
!PointColl categoriesFor: #inject:into:!public! !
!PointColl categoriesFor: #intersection:!public! !
!PointColl categoriesFor: #isEmpty!public! !
!PointColl categoriesFor: #maxPrint!private! !
!PointColl categoriesFor: #newSelection!private! !
!PointColl categoriesFor: #noDifference:!public! !
!PointColl categoriesFor: #notEmpty!public! !
!PointColl categoriesFor: #occurrencesOf:!public! !
!PointColl categoriesFor: #printCyclicRefOn:!private! !
!PointColl categoriesFor: #printOn:!public! !
!PointColl categoriesFor: #printPrefixOn:!private! !
!PointColl categoriesFor: #printSuffixOn:!private! !
!PointColl categoriesFor: #publishedAspects!public! !
!PointColl categoriesFor: #publishedKeyedAspects!public! !
!PointColl categoriesFor: #publishedKeyedAspectsBatchSize!private! !
!PointColl categoriesFor: #rehash!public! !
!PointColl categoriesFor: #reject:!public! !
!PointColl categoriesFor: #remove:!public! !
!PointColl categoriesFor: #remove:ifAbsent:!public! !
!PointColl categoriesFor: #removeAll:!public! !
!PointColl categoriesFor: #select:!public! !
!PointColl categoriesFor: #select:thenCollect:!public! !
!PointColl categoriesFor: #setOrderCollection:!public! !
!PointColl categoriesFor: #size!public! !
!PointColl categoriesFor: #species!public! !
!PointColl categoriesFor: #storeOn:!public! !
!PointColl categoriesFor: #sunitbRemoveAll:!public! !
!PointColl categoriesFor: #symmetricDifference:!public! !
!PointColl categoriesFor: #union:!public! !

!PointColl class methodsFor!

defaultSortAlgorithmClass
	"Answer the class of <PluggableSortAlgorithm> to be used by default in conjunction with a
	user-defined sort block."

	^OrderedCollection defaultSortAlgorithmClass!

icon
	"Answers an Icon that can be used to represent this class"

	^OrderedCollection icon!

new
	^self shouldNotImplement!

new: x
	^self shouldNotImplement!

newInstanceAspect: aSymbol class: aspectClass 
	"Private - Answer a new <Aspect> of the class, aspectClass, and with name, aSymbol, 
    	which is appropriate for representing aspects of the receiver's type."

	^OrderedCollection newInstanceAspect: aSymbol class: aspectClass!

with: element1
	"Answer an instance of the receiver containing the <Object>, element1.
	Implementation Note: The #yourself is definitely required here."
	^self shouldNotImplement
 !

with: element1 with: element2
	"Answer an instance of the receiver containing each of the <Object>
	arguments as its elements"
	^self shouldNotImplement
 !

with: element1 with: element2 with: element3
	"Answer an instance of the receiver containing each of the <Object>
	arguments as its elements."

	^self shouldNotImplement
 !

with: element1 with: element2 with: element3 with: element4
	"Answer an instance of the receiver containing each of the <Object>
	arguments as its elements."

	^self shouldNotImplement
 !

with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
	"Answer an instance of the receiver containing the arguments as its elements"

	^self shouldNotImplement
 !

withAll: newElements
	"Answer a new instance of the receiver containing all of the 
	elements of the <collection>, newElements."

	^self shouldNotImplement!

zwroc: collection
       ^super new createPointCollection: collection! !
!PointColl class categoriesFor: #defaultSortAlgorithmClass!public! !
!PointColl class categoriesFor: #icon!public! !
!PointColl class categoriesFor: #new!public! !
!PointColl class categoriesFor: #new:!public! !
!PointColl class categoriesFor: #newInstanceAspect:class:!private! !
!PointColl class categoriesFor: #with:!public! !
!PointColl class categoriesFor: #with:with:!public! !
!PointColl class categoriesFor: #with:with:with:!public! !
!PointColl class categoriesFor: #with:with:with:with:!public! !
!PointColl class categoriesFor: #with:with:with:with:with:!public! !
!PointColl class categoriesFor: #withAll:!public! !
!PointColl class categoriesFor: #zwroc:!public! !

"Binary Globals"!

