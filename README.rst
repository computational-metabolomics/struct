==============================================
STRUCT: STatistics in R Using Class Templates
==============================================

|Git| |Bioconda| |License| |Coverage|

------------
Overview
------------

``struct`` is an R package that helps you build consistent, reusable data-analysis
workflows using object-oriented templates. In simple terms, it gives you a
standard way to define:

- what inputs an analysis method expects,
- what outputs it returns, and
- how methods are documented and combined into larger pipelines.

This makes code easier to understand, test, and share across collaborators,
especially when projects involve many statistical methods.

------------
Install
------------

Choose one of the installation methods below.

Github
------------

.. code-block:: r

  library(devtools)
  install_github('computational-metabolomics/struct')
  library(struct)

Conda
------------

.. code-block:: command

   conda create -n struct struct -c conda-forge -c bioconda -c computational-metabolomics
   conda activate struct

Then start R and load the package:

.. code-block:: r

   library(struct)
   library(structToolbox)

------------
Quick Start
------------

The example below shows a simple two-step analysis pipeline using objects from
the companion package structToolbox: mean centring followed by PCA.

.. code-block:: r

   # Load struct and companion toolbox
   library(struct)
   library(structToolbox)

   # Example dataset, in struct format
   DE = iris_DatasetExperiment()

   # Build model sequence
   MS = mean_centre() + PCA()

   # Train the model using the data
   MS = model_train(MS,DE)

   # Get predictions for the data using the model
   MS = model_predict(MS,DE)

   # Access results via output
   scores <- MS[2]$scores
   loadings <- MS[2]$loadings

------------
References
------------

.. |Git| image:: https://img.shields.io/badge/repository-GitHub-blue.svg?style=flat&maxAge=3600
   :target: https://github.com/computational-metabolomics/structToolbox

.. |Bioconda| image:: https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat&maxAge=3600
   :target: https://bioconda.github.io/recipes/bioconductor-struct/README.html

.. |License| image:: https://img.shields.io/badge/licence-GNU_v3-teal.svg?style=flat&maxAge=3600
   :target: https://www.gnu.org/licenses/gpl-3.0.html

.. |Coverage| image:: https://codecov.io/gh/computational-metabolomics/struct/branch/master/graph/badge.svg
   :target: https://codecov.io/gh/computational-metabolomics/struct
