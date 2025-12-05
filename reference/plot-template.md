# Template for creating standard plot functions

This is a template showing the standard structure for plot functions.
All plot functions should follow this pattern:

1.  A main exported function that:

    - Validates common arguments

    - Processes theme

    - Handles splits via build_plot()

2.  An atomic function that:

    - Creates the actual ggplot

    - Takes simple, validated inputs

    - Returns a ggplot object
