# Notes for project-specific configuration file

* Configuration is project-specific (cf. `make` file)
* Format as XML; CollateX should validate (using data binding and custom case classes) after reading
* Input is either single JSON file (cf. current CollateX) or filenames (verify)
* Filename information:
  * Ordered list of filenames (paths?)
  * Map from filename to abbreviated siglum
  * Configuration file should indicate whether to compute rendering order or 
    take from filename order in configuration document (which is default)
  * Optional mapping to from filename to color for alignment ribbon visualization
* List of output formats
  * With output filenames or filename conventions
  * Each output filanem has a segmentation specification
  * Orientation is a separate output, and not a switch on a shared output
  * Outputs
    * GraphML (representation of variant graph)
    * TEI XML
    * Interedition XML
    * Graphviz dot
    * SVG of alignment ribbon (done!) 
    * SVG of variant graph (simple and rich versions; would need to 
      run dot from within Scala, or wrap in batch/shell script)
    * ASCII alignment tables (both orientations)
    * HTML alignment tables (both orientations, with color)
    * JSON (not alignment-table JSON)
* Near matching option

No configurable tokenization or normalization; if needed, preprocess and create JSON input.

Web service to allow CollateX within a pipeline? JSON input, JSON output (GraphML? others?).