# Static Assets

This project is used to serve the following static assets:

- `styles.css` (compiled from `scss/styles.scss`)
- `custom.css`
- The bootstrap framework, exposed via `bootstrap.min.js`
- The bootstrap-icos, exposed as the full contents of `bootstrap-icons/font/*`

At the moment the assets must be built locally and check-in to the repository.
To do that, run the following commands:

```sh
yarn install
yarn css
```

And commit the changes.
