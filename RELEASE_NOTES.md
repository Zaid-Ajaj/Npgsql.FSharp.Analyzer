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
