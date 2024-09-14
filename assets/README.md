# Static Assets

This project is used to serve the following static assets:

- `styles.css` (compiled from `scss/styles.scss`)
- `custom.css`
- The bootstrap framework, exposed via `bootstrap.min.js`
- The bootstrap-icos, exposed as the full contents of `bootstrap-icons/font/*`

At the moment the assets must be built locally and check-in to the repository.
To do that, run the following commands:

```sh
npm install --no-save
npm run css
```

The `.dist/` folder is generated automatically by the `npm run css` command and must be commited to the repository.

# Ideas for further extension & refinement

- Install the `glyphicons` fonts as an npm package and serve them from the `assets/` project (currently they are vendored-in in `../fonts/`)
- Use `purgecss` to reduce the size of the stylesheets
- Do not check-in the `dist/` folder to the repository.
  Instead build it as part of the deployment pipeline.
