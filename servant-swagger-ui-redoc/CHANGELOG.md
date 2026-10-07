- 0.3.5.2.5.0
    - Update to ReDoc-2.5.0
    - **Breaking:** ReDoc 2.x drops support for Internet Explorer and other
      legacy browsers, and renames or removes several ReDoc 1.x options.
      Custom index templates that set ReDoc 1.x options should be checked
      against the [ReDoc 2.x documentation](https://redocly.com/docs/redoc/config).
    - The bundle is now `redoc.standalone.js`. It is still served as
      `redoc.min.js` for custom templates that reference the old name.
    - Ship ReDoc's license and the bundle's third-party license notices.

- 0.3.2.1.22.3
    - Update to ReDoc-1.22.3

- 0.3.3.1.22.2
    - Add `swaggerSchemaUIServer'`

- 0.3.2.1.22.2
    - Update to ReDoc-1.22.2
    - Add GHC-8.6 support
    - Drop `servant<0.14` support
