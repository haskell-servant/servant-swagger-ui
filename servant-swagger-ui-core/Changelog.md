# 0.3.6

- Generalize to support arbitrary type instead of hardcoded `Value`.

  `SwaggerSchemaUI` now needs one more parameter which typically is
  either `Swagger` or `OpenApi`.

  To migrate from older version, alter your API type from
  `SwaggerSchemaUI "swagger-ui" "swagger.json"`
  to
  `SwaggerSchemaUI "swagger-ui" "swagger.json" Swagger`

  Or in case of the more generic variant, old
  `SwaggerSchemaUI' "foo-ui" ("foo" :> "swagger.json" :> Get '[JSON] Value)`
  becomes
  `SwaggerSchemaUI' "foo-ui" ("foo" :> "swagger.json" :> Get '[JSON] Swagger)`

# 0.3.5

- Generalize `SwaggerSchemaUI` and `swaggerSchemaUIServerImpl` to support arbitrary `Value` instead
  of hardcoded `Swagger` type.

# 0.3.3

- Add `swaggerSchemaUIServerImpl'`

# 0.3.1

- Support `servant-0.14`
