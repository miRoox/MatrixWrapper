# ``MatrixWrapper` ``

### `Mat`

`Mat@data` treat `data` as a matrix.

`Mat@"Column"@data` treat `data` as a column matrix.

`Mat@"Row"@data` treat `data` as a row matrix.

`Mat[data,patt]` suppose every element in `data` matches `patt`.

### `MatQ`

`MatQ[expr,patt]` check whether `expr` is a correct `Mat` type matrix with pattern constraint `patt`.

`MatQ[patt]` represents an operator form of `MatQ` that can be applied to an expression.

### `MatData`

`MatData[mat]` get raw data of `Mat` type matrix.

### `MatPatternConstraint`

`MatPatternConstraint[mat]` get pattern constraint of `mat`.

### `MatrixFunctor`

`MatrixFunctor[f]` give the matrix form of a scaler function `f`.
