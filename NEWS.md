# stantrackr 0.3.0

### New features

- New S3method for `plot()`
- New function `mapTrack()`

# stantrackr 0.2.0

### New features

- $N$-state HMM implementation with state-depended process correlation parameter $\lambda_n$.
- Option to use the same $\lambda_n$ across all tracks by setting `i_lambda = FALSE`.

### Documentation

- New vignette explaining the model formulation.

### Deprecated features

- Using Stan's optimisation method is deprecated.

# stantarckr 0.1.2

### New features

- New function `getDraws()`

### Documentation

- New vignette `stantrackr_example`

# stantarckr 0.1.1

### New features

- Support antenna-specific `det_range`
- Utilisation of Stan's optimisation algorithms
- Support multiple-variable summaries
