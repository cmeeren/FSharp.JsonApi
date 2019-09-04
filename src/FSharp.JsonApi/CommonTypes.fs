namespace FSharp.JsonApi

type TypeName = string
type FieldName = string
type AttributeName = string
type RelationshipName = string
type Fieldsets = Map<TypeName, Set<FieldName>>
type IncludePath = RelationshipName list
