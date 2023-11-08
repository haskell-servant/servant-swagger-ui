- 0.3.6.1.22.3
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

- 0.3.2.1.22.3
    - Update to ReDoc-1.22.3

- 0.3.3.1.22.2
    - Add `swaggerSchemaUIServer'`

- 0.3.2.1.22.2
    - Update to ReDoc-1.22.2
    - Add GHC-8.6 support
    - Drop `servant<0.14` support
