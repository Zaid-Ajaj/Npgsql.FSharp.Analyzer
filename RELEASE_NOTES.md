#### 3.2.0 - 2020-03-05
* Update FSharp.Analyzers SDK 0.3.1 -> 0.4.0 with named analyzers.

#### 3.1.0 - 2020-03-05
* Update FSharp.Analyzers SDK and compiler services to align types.

#### 3.0.0 - 2020-02-26
* Update for Npgsql.FSharp 3.x to be able to analyze column reading functions as `{type}OrNone` instead of `{type}OrNull`

#### 2.0.0 - 2020-02-24
* Update for Npgsql.FSharp 2.x
* Detect incorrect parameter type with code fixes
* Detect redundant parameters
* Detect nullable types and and suggest using proper function that handle null values

#### 1.9.0 - 2020-02-20
* Optimize number of database calls by reusing the database schema on each invokation
* Detect redundant query parameters in a clear warning message
* Provide code fixes and better suggestions for mismatched query parameters
* Remove duplicate messages about missing parameters
* Refactor and simplify parts of `InformatioSchema` and `SqlAnalysis`

#### 1.8.0 - 2020-02-19
* Provide column name fix suggestions when reading an unknown column

#### 1.7.0 - 2020-02-19
* Read parameters as soon as they written and implement proper code fixes.

#### 1.6.0 - 2020-02-19
* Read queries as soon as they written without expecting `Sql.executeReader`

#### 1.5.0 - 2020-02-19
* Improved syntactic F# analysis in `Sql` module is used in combination with other generic functions

#### 1.4.0 - 2020-02-18
* Enable reading `[<Literal>]` queries from the same module and add docs

#### 1.3.0 - 2020-02-18
* Detect type-mismatch when reading columns of type 'bool' from the database. Simplify parameter mismatch when there is only one parameter.

#### 1.2.0 - 2020-02-18
* Remove warning when there is no query provided (to avoid making a bother-ware analyzer)

#### 1.1.0 - 2020-02-17
* Proper packaging that includes third-party dependencies required for dynamic loading

#### 1.0.0 - 2020-02-17
* Initial release with working SQL analysis including syntax and type-checking
