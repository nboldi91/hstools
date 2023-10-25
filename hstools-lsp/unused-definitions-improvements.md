




 - Split up names: moduleName, definitionName, localName


Find unused optimizations

 - Better way of encoding namespaces
 - Mark local definitions

White-list regexes for used stuff
 - Initially: mains
Names with foreign definitions are automatically used

More exact definition (what is the name of the defined, etc.)

Unused definitions
 - Type definitions, constructors, fields: extra checks

Unused types
 - If all the constructors and fields are unused

Unused constructors
 - Never constructed (constructor / unknown creator (read, deserialize...))
 - Or never consumed (field usage / deconstruct / unknown consumer (lens, show, ...))

Unused fields
 - Always blank in pattern matching / field never used