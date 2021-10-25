open! Core
open! Import
open  Gen_js_api

(** Options Reference: http://dygraphs.com/options.html

    Note: A few callbacks are not bound. Feel free to add them! *)

module Line_pattern : sig
  (** A custom pattern array where the even index is a draw and odd is a space in
      pixels. If null then it draws a solid line. The array should have a even length as
      any odd lengthed array could be expressed as a smaller even length array. This is
      used to create dashed gridlines. *)
  type t = int array

  (** This is the same value as Dygraph.DASHED_LINE.  For reasons that are not
      fully understood (or even partially, let's be honest), the "Dygraph" value
      isn't present in the global namespace when run in node-js, so [@@js.global
      "Dygraph.DASHED_LINE"] will crash the program. *)
  val dashed : t
  [@@js.custom let dashed = [| 7; 2 |]]

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
end

module Which_y_axis : sig
  type t =
    ([ `y1
     | `y2
     ]
     [@js.enum])

  val t_to_js : t -> Ojs.t
end

module Legend : sig
  type t =
    ([ `always
     | `follow
     | `never
     | `onmouseover
     ]
     [@js.enum])

  val t_to_js : t -> Ojs.t
end

module Series_options : sig
  type t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t

  val create
    :  ?axis:Which_y_axis.t
    (** axis http://dygraphs.com/options.html#axis

        Set to either 'y1' or 'y2' to assign a series to a y-axis (primary or
        secondary). Must be set per-series.

        Type: string
        Default: (none)
    *)
    -> ?color:Color.t
    (** color http://dygraphs.com/options.html#color

        A per-series color definition. Used in conjunction with, and overrides, the colors
        option.

        Type: string
        Default: (see description)
    *)
    -> ?drawPoints:bool
    (** drawPoints http://dygraphs.com/options.html#drawPoints

        Draw a small dot at each point, in addition to a line going through the
        point. This makes the individual data points easier to see, but can increase
        visual clutter in the chart. The small dot can be replaced with a custom
        rendering by supplying a drawPointCallback.

        Type: boolean
        Default: false
    *)
    -> ?showInRangeSelector:bool
    (** showInRangeSelector http://dygraphs.com/options.html#showInRangeSelector

        Mark this series for inclusion in the range selector. The mini plot curve
        will be an average of all such series. If this is not specified for any
        series, the default behavior is to average all the visible series. Setting it
        for one series will result in that series being charted alone in the range
        selector. Once it's set for a single series, it needs to be set for all
        series which should be included (regardless of visibility).

        Type: boolean
        Default: null
    *)
    -> ?strokePattern:Line_pattern.t
    (** strokePattern http://dygraphs.com/options.html#strokePattern

        A custom pattern array where the even index is a draw and odd is a space in
        pixels. If null then it draws a solid line. The array should have a even
        length as any odd lengthed array could be expressed as a smaller even length
        array. This is used to create dashed lines.

        Type: array
        Default: null
    *)
    -> ?strokeWidth:float
    (** strokeWidth http://dygraphs.com/options.html#strokeWidth

        The width of the lines connecting data points. This can be
        used to increase the contrast or some graphs.

        Type: float
        Default: 1.0
    *)
    -> unit
    -> t
  [@@js.builder]
end

module Series : sig
  type t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t

  val create : (string * Series_options.t) list -> t
  [@@js.custom
    let create data =
      data |> List.Assoc.map ~f:Series_options.t_to_js |> Array.of_list |> Ojs.obj
    ;;]
end

module Opts : sig
  (** a function which provides access to various options on the dygraph,
      e.g. opts('labelsKMB').

      See [axisLabelFormatter] in [Axis_options.create] for an example of how you might
      get your hands on one of these.

      For an example of how to use this, see [X_axis_mapping.t]. *)
  type t
end

module Axis_options : sig
  type t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t

  val create
    :  ?axisLabelFormatter:(Number_or_js_date.t -> Granularity.t -> Opts.t -> string)
    (** axisLabelFormatter http://dygraphs.com/options.html#axisLabelFormatter

        Function to call to format the tick values that appear along an axis. This is
        usually set on a per-axis basis.

        Type: function(number or Date, granularity, opts, dygraph)

        number or date: Either a number (for a numeric axis) or a Date object (for a date
        axis)

        granularity: specifies how fine-grained the axis is. For date axes, this is a
        reference to the time granularity enumeration, defined in dygraph-tickers.js,
        e.g. Dygraph.WEEKLY.

        opts: a function which provides access to various options on the dygraph,
        e.g. opts('labelsKMB').

        dygraph: the referenced graph

        Default: Depends on the data type
        Gallery Samples: NONE
        Other Examples: value-axis-formatters x-axis-formatter
    *)
    -> ?valueFormatter:(float -> Opts.t -> string)
    (** valueFormatter http://dygraphs.com/options.html#valueFormatter

        Function to provide a custom display format for the values displayed on
        mouseover. This does not affect the values that appear on tick marks next to the
        axes. To format those, see axisLabelFormatter. This is usually set on a per-axis
        basis. .

        Type: function(num or millis, opts, seriesName, dygraph, row, col)

        num_or_millis: The value to be formatted. This is always a number. For date axes,
        it's millis since epoch. You can call new Date(millis) to get a Date object.

        opts: This is a function you can call to access various options
        (e.g. opts('labelsKMB')). It returns per-axis values for the option when
        available.

        seriesName: The name of the series from which the point came, e.g. 'X', 'Y', 'A',
        etc.

        dygraph: The dygraph object for which the formatting is being done

        row: The row of the data from which this point comes. g.getValue(row, 0) will
        return the x-value for this point.

        col: The column of the data from which this point comes. g.getValue(row, col) will
        return the original y-value for this point. This can be used to get the full
        confidence interval for the point, or access un-rolled values for the point.

        Default: Depends on the type of your data.
        Gallery Samples: NONE
        Other Examples: hairlines labelsKMB multi-scale value-axis-formatters
    *)
    -> ?axisLabelWidth:int
    (** axisLabelWidth http://dygraphs.com/options.html#axisLabelWidth

        Width (in pixels) of the containing divs for x- and y-axis labels. For the
        y-axis, this also controls the width of the y-axis. Note that for the x-axis,
        this is independent from pixelsPerLabel, which controls the spacing between
        labels.

        Type: integer
        Default: 50 (y-axis), 60 (x-axis)
    *)
    -> ?axisLineColor:Color.t
    (** axisLineColor http://dygraphs.com/options.html#axisLineColor

        Color of the x- and y-axis lines. Accepts any value which the HTML canvas
        strokeStyle attribute understands, e.g. 'black' or 'rgb(0, 100, 255)'.

        Type: string
        Default: black
    *)
    -> ?axisLineWidth:float
    (** axisLineWidth http://dygraphs.com/options.html#axisLineWidth

        Thickness (in pixels) of the x- and y-axis lines.

        Type: float
        Default: 0.3
    *)
    -> ?axisTickSize:float
    (** axisTickSize http://dygraphs.com/options.html#axisTickSize

        The size of the line to display next to each tick mark on x- or y-axes.

        Type: number
        Default: 3.0
    *)
    -> ?drawAxis:bool
    (** drawAxis http://dygraphs.com/options.html#drawAxis

        Whether to draw the specified axis. This may be set on a per-axis basis to
        define the visibility of each axis separately. Setting this to false also
        prevents axis ticks from being drawn and reclaims the space for the chart
        grid/lines.

        Type: boolean
        Default: true for x and y, false for y2
    *)
    -> ?includeZero:bool
    (** includeZero http://dygraphs.com/options.html#includeZero

        Usually, dygraphs will use the range of the data plus some padding to set the
        range of the y-axis. If this option is set, the y-axis will always include zero,
        typically as the lowest value. This can be used to avoid exaggerating the
        variance in the data

        Type: boolean
        Default: false
    *)
    -> ?independentTicks:bool
    (** independentTicks http://dygraphs.com/options.html#independentTicks

        Only valid for y and y2, has no effect on x: This option defines whether the y
        axes should align their ticks or if they should be independent. Possible
        combinations: 1.) y=true, y2=false (default): y is the primary axis and the y2
        ticks are aligned to the the ones of y. (only 1 grid) 2.) y=false, y2=true: y2
        is the primary axis and the y ticks are aligned to the the ones of y2. (only 1
        grid) 3.) y=true, y2=true: Both axis are independent and have their own
        ticks. (2 grids) 4.) y=false, y2=false: Invalid configuration causes an error.

        Type: boolean
        Default: true for y, false for y2
    *)
    -> ?logscale:bool
    (** logscale http://dygraphs.com/options.html#logscale

        When set for the y-axis or x-axis, the graph shows that axis in log scale. Any
        values less than or equal to zero are not displayed. Showing log scale with
        ranges that go below zero will result in an unviewable graph. Not compatible
        with showZero. connectSeparatedPoints is ignored. This is ignored for date-based
        x-axes.

        Type: boolean
        Default: false
    *)
    -> ?pixelsPerLabel:int
    (** pixelsPerLabel http://dygraphs.com/options.html#pixelsPerLabel

        Number of pixels to require between each x- and y-label. Larger values will yield
        a sparser axis with fewer ticks. This is set on a per-axis basis.

        Type: integer
        Default: 70 (x-axis) or 30 (y-axes) *)
    -> ?valueRange:Range.Spec.t
    (** valueRange http://dygraphs.com/options.html#valueRange

        Explicitly set the vertical range of the graph to [low, high]. This may be set on
        a per-axis basis to define each y-axis separately. If either limit is unspecified,
        it will be calculated automatically (e.g. [null, 30] to automatically calculate
        just the lower bound)

        Type: Array of two numbers
        Default: Full range of the input is shown
    *)
    -> ?drawGrid:bool
    (** drawGrid http://dygraphs.com/options.html#drawGrid

        Whether to display gridlines in the chart. This may be set on a per-axis
        basis to define the visibility of each axis' grid separately.

        Type: boolean
        Default: true for x and y, false for y2
    *)
    -> ?gridLineColor:Color.t
    (** gridLineColor http://dygraphs.com/options.html#gridLineColor

        The color of the gridlines. This may be set on a per-axis basis to define
        each axis' grid separately.

        Type: red, blue
        Default: rgb(128,128,128)
    *)
    -> ?gridLinePattern:Line_pattern.t
    (** gridLinePattern http://dygraphs.com/options.html#gridLinePattern

        A custom pattern array where the even index is a draw and odd is a space in
        pixels. If null then it draws a solid line. The array should have a even
        length as any odd lengthed array could be expressed as a smaller even length
        array. This is used to create dashed gridlines.

        Type: array
        Default: null
    *)
    -> ?gridLineWidth:float
    (** gridLineWidth http://dygraphs.com/options.html#gridLineWidth

        Thickness (in pixels) of the gridlines drawn under the chart. The
        vertical/horizontal gridlines can be turned off entirely by using the
        drawGrid option. This may be set on a per-axis basis to define each axis'
        grid separately.

        Type: float
        Default: 0.3
    *)
    -> ?pixelsPerLabel:int
    (** pixelsPerLabel http://dygraphs.com/options.html#pixelsPerLabel

        Number of pixels to require between each x- and y-label. Larger values will
        yield a sparser axis with fewer ticks. This is set on a per-axis basis.

        Type: integer
        Default: 70 (x-axis) or 30 (y-axes)
    *)
    -> unit
    -> t
  [@@js.builder]
end

module Axes : sig
  type t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t

  val create  : ?x:Axis_options.t -> ?y:Axis_options.t -> ?y2:Axis_options.t -> unit -> t
  [@@js.builder]
end

module Highlight_series_options : sig
  type t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t

  val create
    :  ?highlightCircleSize:int
    (** highlightCircleSize http://dygraphs.com/options.html#highlightCircleSize
        The size in pixels of the dot drawn over highlighted points.

        Type: integer
        Default: 3
    *)
    -> ?strokeWidth:float
    (** strokeWidth http://dygraphs.com/options.html#strokeWidth

        The width of the lines connecting data points. This can be used to increase the
        contrast or some graphs.

        Type: float
        Default: 1.0
    *)
    -> ?strokeBorderWidth:float
    (** strokeBorderWidth http://dygraphs.com/options.html#strokeBorderWidth

        Draw a border around graph lines to make crossing lines more easily
        distinguishable. Useful for graphs with many lines.

        Type: float
        Default: null
    *)
    -> unit
    -> t
  [@@js.builder]
end

type t

val t_of_js : Ojs.t -> t
val t_to_js : t -> Ojs.t

[@@@ocamlformat "disable"]

val create
  :  ?axisLabelFontSize:int
  (** Axis display:

      axisLabelFontSize http://dygraphs.com/options.html#LabelFontSize
      Size of the font (in pixels) to use in the axis labels, both x- and y-axis.

      Type: integer
      Default: 14
  *)
  -> ?axisLabelWidth:int
  (** axisLabelWidth http://dygraphs.com/options.html#axisLabelWidth

      Width (in pixels) of the containing divs for x- and y-axis labels. For the
      y-axis, this also controls the width of the y-axis. Note that for the x-axis,
      this is independent from pixelsPerLabel, which controls the spacing between
      labels.

      Type: integer
      Default: 50 (y-axis), 60 (x-axis)
  *)
  -> ?axisLineColor:Color.t
  (** axisLineColor http://dygraphs.com/options.html#axisLineColor

      Color of the x- and y-axis lines. Accepts any value which the HTML canvas
      strokeStyle attribute understands, e.g. 'black' or 'rgb(0, 100, 255)'.

      Type: string
      Default: black
  *)
  -> ?axisLineWidth:float
  (** axisLineWidth http://dygraphs.com/options.html#axisLineWidth

      Thickness (in pixels) of the x- and y-axis lines.

      Type: float
      Default: 0.3
  *)
  -> ?axisTickSize:float
  (** axisTickSize http://dygraphs.com/options.html#axisTickSize

      The size of the line to display next to each tick mark on x- or y-axes.

      Type: number
      Default: 3.0
  *)
  -> ?dateWindow:Range.t
  (** dateWindow http://dygraphs.com/options.html#dateWindow

      Initially zoom in on a section of the graph. Is of the form [earliest, latest],
      where earliest/latest are milliseconds since epoch. If the data for the x-axis
      is numeric, the values in dateWindow must also be numbers.

      Type: Array of two numbers
      Default: Full range of the input is shown
  *)
  -> ?drawAxesAtZero:bool
  (** drawAxesAtZero http://dygraphs.com/options.html#drawAxesAtZero

      When set, draw the X axis at the Y=0 position and the Y axis at the X=0 position
      if those positions are inside the graph's visible area. Otherwise, draw the axes
      at the bottom or left graph edge as usual.

      Type: boolean
      Default: false
  *)
  -> ?drawAxis:bool
  (** drawAxis http://dygraphs.com/options.html#drawAxis

      Whether to draw the specified axis. This may be set on a per-axis basis to
      define the visibility of each axis separately. Setting this to false also
      prevents axis ticks from being drawn and reclaims the space for the chart
      grid/lines.

      Type: boolean
      Default: true for x and y, false for y2
  *)
  -> ?includeZero:bool
  (** includeZero http://dygraphs.com/options.html#includeZero

      Usually, dygraphs will use the range of the data plus some padding to set the
      range of the y-axis. If this option is set, the y-axis will always include zero,
      typically as the lowest value. This can be used to avoid exaggerating the
      variance in the data

      Type: boolean
      Default: false
  *)
  -> ?logscale:bool
  (** logscale http://dygraphs.com/options.html#logscale

      When set for the y-axis or x-axis, the graph shows that axis in log scale. Any
      values less than or equal to zero are not displayed. Showing log scale with
      ranges that go below zero will result in an unviewable graph. Not compatible
      with showZero. connectSeparatedPoints is ignored. This is ignored for date-based
      x-axes.

      Type: boolean
      Default: false
  *)
  -> ?panEdgeFraction:float
  (** panEdgeFraction http://dygraphs.com/options.html#panEdgeFraction

      A value representing the farthest a graph may be panned, in percent of the
      display. For example, a value of 0.1 means that the graph can only be panned 10%
      passed the edges of the displayed values. null means no bounds.

      Type: float
      Default: null
  *)
  -> ?valueRange:Range.Spec.t
  (** valueRange http://dygraphs.com/options.html#valueRange

      Explicitly set the vertical range of the graph to [low, high]. This may be set on
      a per-axis basis to define each y-axis separately. If either limit is unspecified,
      it will be calculated automatically (e.g. [null, 30] to automatically calculate
      just the lower bound)

      Type: Array of two numbers
      Default: Full range of the input is shown
  *)
  -> ?xAxisHeight:int
  (** xAxisHeight http://dygraphs.com/options.html#xAxisHeight

      Height, in pixels, of the x-axis. If not set explicitly, this is computed based on
      axisLabelFontSize and axisTickSize.

      Type: integer
      Default: (null)
  *)
  -> ?xRangePad:float
  (** xRangePad http://dygraphs.com/options.html#xRangePad

      Add the specified amount of extra space (in pixels) around the X-axis value range
      to ensure points at the edges remain visible.

      Type: float
      Default: 0
  *)
  -> ?yRangePad:float
  (** yRangePad http://dygraphs.com/options.html#yRangePad

      If set, add the specified amount of extra space (in pixels) around the Y-axis
      value range to ensure points at the edges remain visible. If unset, use the
      traditional Y padding algorithm.

      Type: float
      Default: null
  *)
  -> ?customBars:bool
  (** customBars http://dygraphs.com/options.html#customBars

      When set, parse each CSV cell as "low;middle;high". Error bars will be drawn for
      each point between low and high, with the series itself going through middle.

      Type: boolean
      Default: false
  *)
  -> ?errorBars:bool
  (** errorBars http://dygraphs.com/options.html#errorBars

      Does the data contain standard deviations? Setting this to true alters the input
      format (see above).

      Type: boolean
      Default: false
  *)
  -> ?fractions:bool
  (** fractions http://dygraphs.com/options.html#fractions

      When set, attempt to parse each cell in the CSV file as "a/b", where a and b are
      integers. The ratio will be plotted. This allows computation of Wilson confidence
      intervals (see below).

      Type: boolean
      Default: false
  *)
  -> ?title:string
  (** title http://dygraphs.com/options.html#title

      Text to display above the chart. You can supply any HTML for this value, not just
      text. If you wish to style it using CSS, use the 'dygraph-label' or
      'dygraph-title' classes.

      Type: string
      Default: null
  *)
  -> ?titleHeight:int
  (** titleHeight http://dygraphs.com/options.html#titleHeight

      Height of the chart title, in pixels. This also controls the default font size of
      the title. If you style the title on your own, this controls how much space is set
      aside above the chart for the title's div.

      Type: integer
      Default: 18
  *)
  -> ?xLabelHeight:int
  (** xLabelHeight http://dygraphs.com/options.html#xLabelHeight

      Height of the x-axis label, in pixels. This also controls the default font size of
      the x-axis label. If you style the label on your own, this controls how much space
      is set aside below the chart for the x-axis label's div.

      Type: integer
      Default: 18
  *)
  -> ?xlabel:string
  (** xlabel http://dygraphs.com/options.html#xlabel

      Text to display below the chart's x-axis. You can supply any HTML for this value,
      not just text. If you wish to style it using CSS, use the 'dygraph-label' or
      'dygraph-xlabel' classes.

      Type: string
      Default: null
  *)
  -> ?y2label:string
  (** y2label http://dygraphs.com/options.html#y2label

      Text to display to the right of the chart's secondary y-axis. This label is only
      displayed if a secondary y-axis is present. See this test for an example of how to
      do this. The comments for the 'ylabel' option generally apply here as well. This
      label gets a 'dygraph-y2label' instead of a 'dygraph-ylabel' class.

      Type: string
      Default: null
  *)
  -> ?yLabelWidth:int
  (** yLabelWidth http://dygraphs.com/options.html#yLabelWidth

      Width of the div which contains the y-axis label. Since the y-axis label appears
      rotated 90 degrees, this actually affects the height of its div.

      Type: integer
      Default: 18
  *)
  -> ?ylabel:string
  (** ylabel http://dygraphs.com/options.html#ylabel

      Text to display to the left of the chart's y-axis. You can supply any HTML for
      this value, not just text. If you wish to style it using CSS, use the
      'dygraph-label' or 'dygraph-ylabel' classes. The text will be rotated 90 degrees
      by default, so CSS rules may behave in unintuitive ways. No additional space is
      set aside for a y-axis label. If you need more space, increase the width of the
      y-axis tick labels using the yAxisLabelWidth option. If you need a wider div for
      the y-axis label, either style it that way with CSS (but remember that it's
      rotated, so width is controlled by the 'height' property) or set the yLabelWidth
      option.

      Type: string
      Default: null
  *)
  -> ?axes:Axes.t
  (** axes http://dygraphs.com/options.html#axes

      Defines per-axis options. Valid keys are 'x', 'y' and 'y2'. Only some options may
      be set on a per-axis basis. If an option may be set in this way, it will be noted
      on this page. See also documentation on per-series and per-axis options.

      Type: Object
      Default: null
  *)
  -> ?connectSeparatedPoints:bool
  (** connectSeparatedPoints http://dygraphs.com/options.html#connectSeparatedPoints

      Usually, when Dygraphs encounters a missing value in a data series, it interprets
      this as a gap and draws it as such. If, instead, the missing values represents an
      x-value for which only a different series has data, then you'll want to connect
      the dots by setting this to true. To explicitly include a gap with this option
      set, use a value of NaN.

      Type: boolean
      Default: false
  *)
  -> ?drawGapEdgePoints:bool
  (** drawGapEdgePoints http://dygraphs.com/options.html#drawGapEdgePoints

      Draw points at the edges of gaps in the data. This improves visibility of small
      data segments or other data irregularities.

      Type: boolean
      Default: false
  *)
  -> ?drawPoints:bool
  (** drawPoints http://dygraphs.com/options.html#drawPoints

      Draw a small dot at each point, in addition to a line going through the
      point. This makes the individual data points easier to see, but can increase
      visual clutter in the chart. The small dot can be replaced with a custom
      rendering by supplying a drawPointCallback.

      Type: boolean
      Default: false
  *)
  -> ?fillGraph:bool
  (** fillGraph http://dygraphs.com/options.html#fillGraph

      Should the area underneath the graph be filled? This option is not compatible
      with error bars. This may be set on a per-series basis.

      Type: boolean
      Default: false
  *)
  -> ?pointSize:int
  (** pointSize http://dygraphs.com/options.html#pointSize

      The size of the dot to draw on each point in pixels (see drawPoints). A dot is always
      drawn when a point is "isolated", i.e. there is a missing point on either side of
      it. This also controls the size of those dots.

      Type: integer
      Default: 1
  *)
  -> ?stackedGraph:bool
  (** stackedGraph http://dygraphs.com/options.html#stackedGraph

      If set, stack series on top of one another rather than drawing them
      independently. The first series specified in the input data will wind up on
      top of the chart and the last will be on bottom. NaN values are drawn as
      white areas without a line on top, see stackedGraphNaNFill for details.

      Type: boolean
      Default: false
  *)
  -> ?stackedGraphNaNFill:string
  (** stackedGraphNaNFill http://dygraphs.com/options.html#stackedGraphNaNFill

      Controls handling of NaN values inside a stacked graph. NaN values are
      interpolated/extended for stacking purposes, but the actual point value
      remains NaN in the legend display. Valid option values are "all" (interpolate
      internally, repeat leftmost and rightmost value as needed), "inside"
      (interpolate internally only, use zero outside leftmost and rightmost value),
      and "none" (treat NaN as zero everywhere).

      Type: string
      Default: all
  *)
  -> ?stepPlot:bool
  (** stepPlot http://dygraphs.com/options.html#stepPlot

      When set, display the graph as a step plot instead of a line plot. This
      option may either be set for the whole graph or for single series.

      Type: boolean
      Default: false
  *)
  -> ?strokeBorderColor:Color.t
  (** strokeBorderColor http://dygraphs.com/options.html#strokeBorderColor

      Color for the line border used if strokeBorderWidth is set.

      Type: string
      Default: white
  *)
  -> ?strokeBorderWidth:float
  (** strokeBorderWidth http://dygraphs.com/options.html#strokeBorderWidth

      Draw a border around graph lines to make crossing lines more easily
      distinguishable. Useful for graphs with many lines.

      Type: float
      Default: null
  *)
  -> ?strokePattern:Line_pattern.t
  (** strokePattern http://dygraphs.com/options.html#strokePattern

      A custom pattern array where the even index is a draw and odd is a space in
      pixels. If null then it draws a solid line. The array should have a even
      length as any odd lengthed array could be expressed as a smaller even length
      array. This is used to create dashed lines.

      Type: array
      Default: null
  *)
  -> ?strokeWidth:float
  (** strokeWidth http://dygraphs.com/options.html#strokeWidth

      The width of the lines connecting data points. This can be
      used to increase the contrast or some graphs.

      Type: float
      Default: 1.0
  *)
  -> ?visibility:bool list
  (** visibility http://dygraphs.com/options.html#visibility

      Which series should initially be visible? Once the Dygraph has been
      constructed, you can access and modify the visibility of each series using
      the visibility and setVisibility methods.

      Type: Array of booleans
      Default: [true, true, ...]
  *)
  -> ?colorSaturation:float
  (** colorSaturation http://dygraphs.com/options.html#colorSaturation

      If colors is not specified, saturation of the
      automatically-generated data series colors.

      Type: float (0.0 - 1.0)
      Default: 1.0
  *)
  -> ?colorValue:float
  (** colorValue http://dygraphs.com/options.html#colorValue

      If colors is not specified, value of the data series colors, as in
      hue/saturation/value. (0.0-1.0, default 0.5)

      Type: float (0.0 - 1.0)
      Default: 1.0
  *)
  -> ?colors:Color.t array
  (** colors http://dygraphs.com/options.html#colors

      List of colors for the data series. These can be of the form "#AABBCC" or
      "rgb(255,100,200)" or "yellow", etc. If not specified, equally-spaced points
      around a color wheel are used. Overridden by the 'color' option.

      Type: array
      Default: (see description)
  *)
  -> ?fillAlpha:float
  (** fillAlpha http://dygraphs.com/options.html#fillAlpha

      Error bars (or custom bars) for each series are drawn in the same color as
      the series, but with partial transparency. This sets the transparency. A
      value of 0.0 means that the error bars will not be drawn, whereas a value of
      1.0 means that the error bars will be as dark as the line for the series
      itself. This can be used to produce chart lines whose thickness varies at
      each point.

      Type: float (0.0 - 1.0)
      Default: 0.15
  *)
  -> ?rollPeriod:int
  (** rollPeriod http://dygraphs.com/options.html#rollPeriod

      Number of days over which to average data. Discussed extensively above.

      Type: integer >= 1
      Default: 1
  *)
  -> ?sigma:float
  (** sigma http://dygraphs.com/options.html#sigma

      When errorBars is set, shade this many standard deviations above/below each
      point.

      Type: float
      Default: 2.0
  *)
  -> ?wilsonInterval:bool
  (** wilsonInterval http://dygraphs.com/options.html#wilsonInterval

      Use in conjunction with the "fractions" option. Instead of plotting +/- N
      standard deviations, dygraphs will compute a Wilson confidence interval and
      plot that. This has more reasonable behavior for ratios close to 0 or 1.

      Type: boolean
      Default: true
  *)
  -> ?drawGrid:bool
  (** drawGrid http://dygraphs.com/options.html#drawGrid

      Whether to display gridlines in the chart. This may be set on a per-axis
      basis to define the visibility of each axis' grid separately.

      Type: boolean
      Default: true for x and y, false for y2
  *)
  -> ?gridLineColor:Color.t
  (** gridLineColor http://dygraphs.com/options.html#gridLineColor

      The color of the gridlines. This may be set on a per-axis basis to define
      each axis' grid separately.

      Type: red, blue
      Default: rgb(128,128,128)
  *)
  -> ?gridLinePattern:Line_pattern.t
  (** gridLinePattern http://dygraphs.com/options.html#gridLinePattern

      A custom pattern array where the even index is a draw and odd is a space in
      pixels. If null then it draws a solid line. The array should have a even
      length as any odd lengthed array could be expressed as a smaller even length
      array. This is used to create dashed gridlines.

      Type: array
      Default: null
  *)
  -> ?gridLineWidth:float
  (** gridLineWidth http://dygraphs.com/options.html#gridLineWidth

      Thickness (in pixels) of the gridlines drawn under the chart. The
      vertical/horizontal gridlines can be turned off entirely by using the
      drawGrid option. This may be set on a per-axis basis to define each axis'
      grid separately.

      Type: float
      Default: 0.3
  *)
  -> ?animatedZooms:bool
  (** animatedZooms http://dygraphs.com/options.html#animatedZooms

      Set this option to animate the transition between zoom windows. Applies to
      programmatic and interactive zooms. Note that if you also set a drawCallback,
      it will be called several times on each zoom. If you set a zoomCallback, it
      will only be called after the animation is complete.

      Type: boolean
      Default: false
  *)
  -> ?hideOverlayOnMouseOut:bool
  (** hideOverlayOnMouseOut http://dygraphs.com/options.html#hideOverlayOnMouseOut

      Whether to hide the legend when the mouse leaves the chart area.

      Type: boolean
      Default: true
  *)
  -> ?highlightCircleSize:int
  (** highlightCircleSize http://dygraphs.com/options.html#highlightCircleSize

      The size in pixels of the dot drawn over highlighted points.

      Type: integer
      Default: 3
  *)
  -> ?highlightSeriesBackgroundAlpha:float
  (** highlightSeriesBackgroundAlpha http://dygraphs.com/options.html#highlightSeriesBackgroundAlpha

      Fade the background while highlighting series. 1=fully visible background
      (disable fading), 0=hiddden background (show highlighted series only).

      Type: float
      Default: 0.5
  *)
  -> ?highlightSeriesBackgroundColor:Color.t
  (** highlightSeriesBackgroundColor http://dygraphs.com/options.html#highlightSeriesBackgroundColor

      Sets the background color used to fade out the series in conjunction with
      'highlightSeriesBackgroundAlpha'.

      Type: string
      Default: rgb(255, 255, 255)
  *)
  -> ?highlightSeriesOpts:Highlight_series_options.t
  (** highlightSeriesOpts http://dygraphs.com/options.html#highlightSeriesOpts

      When set, the options from this object are applied to the timeseries closest to
      the mouse pointer for interactive highlighting. See also
      'highlightCallback'. Example: highlightSeriesOpts: { strokeWidth: 3 }.

      Type: Object
      Default: null
  *)
  -> ?showLabelsOnHighlight:bool
  (** showLabelsOnHighlight http://dygraphs.com/options.html#showLabelsOnHighlight

      Whether to show the legend upon mouseover.

      Type: boolean
      Default: true
  *)
  -> ?showRoller:bool
  (** showRoller http://dygraphs.com/options.html#showRoller

      If the rolling average period text box should be shown.

      Type: boolean
      Default: false
  *)
  -> ?hideOverlayOnMouseOut:bool
  (** hideOverlayOnMouseOut http://dygraphs.com/options.html#hideOverlayOnMouseOut

      Whether to hide the legend when the mouse leaves the chart area.

      Type: boolean
      Default: true
  *)
  -> ?labels:string list
  (** labels http://dygraphs.com/options.html#labels

      A name for each data series, including the independent (X) series. For CSV
      files and DataTable objections, this is determined by context. For raw data,
      this must be specified. If it is not, default values are supplied and a
      warning is logged.

      Type: array
      Default: ["X", "Y1", "Y2", ...]*
  *)
  -> ?labelsDiv_string:(string[@js "labelsDiv"])
  (** labelsDiv http://dygraphs.com/options.html#labelsDiv

      Show data labels in an external div, rather than on the graph. This value can
      either be a div element or a div id.

      Type: DOM element or string
      Default: null
  *)
  -> ?labelsDiv_el:(Native_node.t[@js "labelsDiv"])
  (** labelsDiv http://dygraphs.com/options.html#labelsDiv

      Show data labels in an external div, rather than on the graph. This value can
      either be a div element or a div id.

      Type: DOM element or string
      Default: null
  *)
  -> ?labelsSeparateLines:bool
  (** labelsSeparateLines http://dygraphs.com/options.html#labelsSeparateLines

      Put <br/> between lines in the label string. Often used in
      conjunction with labelsDiv.

      Type: boolean
      Default: false
  *)
  -> ?labelsShowZeroValues:bool
  (** labelsShowZeroValues http://dygraphs.com/options.html#labelsShowZeroValues

      Show zero value labels in the labelsDiv.

      Type: boolean
      Default: true
  *)
  -> ?legend:Legend.t
  (** legend http://dygraphs.com/options.html#legend

      When to display the legend. By default, it only appears when a user mouses
      over the chart. Set it to "always" to always display a legend of some
      sort. When set to "follow", legend follows highlighted points.

      Type: string
      Default: onmouseover
  *)
  -> ?legendFormatter:(Legend_data.t -> string)
  (** legendFormatter http://dygraphs.com/options.html#legendFormatter

      Set this to supply a custom formatter for the legend. See this comment and the
      legendFormatter demo for usage.

      Type: function(data): string
      Default: null
  *)
  -> ?showLabelsOnHighlight:bool
  (** showLabelsOnHighlight http://dygraphs.com/options.html#showLabelsOnHighlight

      Whether to show the legend upon mouseover.

      Type: boolean
      Default: true
  *)
  -> ?height:int
  (** height http://dygraphs.com/options.html#height

      Height, in pixels, of the chart. If the container div has been explicitly
      sized, this will be ignored.

      Type: integer
      Default: 320
  *)
  -> ?clickCallback:(evt:Ojs.t -> x:float -> points:Point.t array -> unit)
  (** clickCallback http://dygraphs.com/options.html#clickCallback

      A function to call when the canvas is clicked.

      Type: function(e, x, points)
      e: The event object for the click
      x: The x value that was clicked (for dates, this is milliseconds since epoch)
      points: The closest points along that date. See Point properties for details.
      Default: null
      Gallery Samples: callbacks highlighted-series
      Other Examples: callback
  *)
  -> ?highlightCallback:(evt:Ojs.t -> x:float -> points:Point.t array -> row:int -> seriesName:string option -> unit)
  (** highlightCallback http://dygraphs.com/options.html#highlightCallback

      When set, this callback gets called every time a new point is highlighted.

      Type: function(event, x, points, row, seriesName)
      event: the JavaScript mousemove event
      x: the x-coordinate of the highlighted points
      points: an array of highlighted points: [ {name: 'series', yval: y-value}, … ]
      row: integer index of the highlighted row in the data table, starting from 0
      seriesName: name of the highlighted series, only present if highlightSeriesOpts is set.
      Default: null
  *)
  -> ?unhighlightCallback:(evt:Ojs.t -> unit)
  (** unhighlightCallback http://dygraphs.com/options.html#unhighlightCallback

      When set, this callback gets called every time the user stops highlighting any
      point by mousing out of the graph.

      Type: function(event)
      event: the mouse event
      Default: null
  *)
  -> ?pointClickCallback:(evt:Ojs.t -> point:Point.t -> unit)
  (** pointClickCallback http://dygraphs.com/options.html#pointClickCallback

      A function to call when a data point is clicked. and the point that was
      clicked.

      Type: function(e, point)
      e: the event object for the click
      point: the point that was clicked See Point properties for details
      Default: null
  *)
  -> ?underlayCallback:(context:Canvas_rendering_context_2D.t -> area:Area.t -> dygraph:Ojs.t -> unit)
  (** underlayCallback http://dygraphs.com/options.html#underlayCallback

      When set, this callback gets called before the chart is drawn. It details on how
      to use this.

      Type: function(context, area, dygraph)
      context: the canvas drawing context on which to draw
      area: An object with {x,y,w,h} properties describing the drawing area.
      dygraph: the reference graph
      Default: null
  *)
  -> ?zoomCallback:(xmin:float -> xmax:float -> yRanges:Range.t array -> unit)
  (** zoomCallback http://dygraphs.com/options.html#zoomCallback

      A function to call when the zoom window is changed (either by zooming in or
      out). When animatedZooms is set, zoomCallback is called once at the end of the
      transition (it will not be called for intermediate frames).

      Type: function(minDate, maxDate, yRanges)
      minDate: milliseconds since epoch
      maxDate: milliseconds since epoch.
      yRanges: is an array of [bottom, top] pairs, one for each y-axis.
      Default: null
  *)
  -> ?pixelRatio:float
  (** pixelRatio http://dygraphs.com/options.html#pixelRatio

      Overrides the pixel ratio scaling factor for the canvas's 2d
      context. Ordinarily, this is set to the devicePixelRatio /
      (context.backingStoreRatio || 1), so on mobile devices, where the
      devicePixelRatio can be somewhere around 3, performance can be improved by
      overriding this value to something less precise, like 1, at the expense of
      resolution.

      Type: float
      Default: (devicePixelRatio / context.backingStoreRatio)
  *)
  -> ?rightGap:int
  (** rightGap http://dygraphs.com/options.html#rightGap

      Number of pixels to leave blank at the right edge of the Dygraph. This makes
      it easier to highlight the right-most data point.

      Type: integer
      Default: 5
  *)
  -> ?width:int
  (** width http://dygraphs.com/options.html#width

      Width, in pixels, of the chart. If the container div has been explicitly
      sized, this will be ignored.

      Type: integer
      Default: 480
  *)
  -> ?rangeSelectorAlpha:float
  (** rangeSelectorAlpha http://dygraphs.com/options.html#rangeSelectorAlpha

      The transparency of the veil that is drawn over the unselected portions of
      the range selector mini plot. A value of 0 represents full transparency and
      the unselected portions of the mini plot will appear as normal. A value of 1
      represents full opacity and the unselected portions of the mini plot will be
      hidden.

      Type: float (0.0 - 1.0)
      Default: 0.6
  *)
  -> ?rangeSelectorBackgroundLineWidth:float
  (** rangeSelectorBackgroundLineWidth http://dygraphs.com/options.html#rangeSelectorBackgroundLineWidth

      The width of the lines below and on both sides of the range selector mini
      plot.

      Type: float
      Default: 1
  *)
  -> ?rangeSelectorBackgroundStrokeColor:Color.t
  (** rangeSelectorBackgroundStrokeColor http://dygraphs.com/options.html#rangeSelectorBackgroundStrokeColor

      The color of the lines below and on both sides of the range selector mini
      plot. This can be of the form "#AABBCC" or "rgb(255,100,200)" or "yellow".

      Type: string
      Default: gray
  *)
  -> ?rangeSelectorForegroundLineWidth:float
  (** rangeSelectorForegroundLineWidth http://dygraphs.com/options.html#rangeSelectorForegroundLineWidth

      The width the lines in the interactive layer of the range selector.

      Type: float
      Default: 1
  *)
  -> ?rangeSelectorForegroundStrokeColor:Color.t
  (** rangeSelectorForegroundStrokeColor http://dygraphs.com/options.html#rangeSelectorForegroundStrokeColor

      The color of the lines in the interactive layer of the range selector. This
      can be of the form "#AABBCC" or "rgb(255,100,200)" or "yellow".

      Type: string
      Default: black
  *)
  -> ?rangeSelectorHeight:int
  (** rangeSelectorHeight http://dygraphs.com/options.html#rangeSelectorHeight

      Height, in pixels, of the range selector widget. This option can only be
      specified at Dygraph creation time.

      Type: integer
      Default: 40
  *)
  -> ?rangeSelectorPlotFillColor:Color.t
  (** rangeSelectorPlotFillColor http://dygraphs.com/options.html#rangeSelectorPlotFillColor

      The range selector mini plot fill color. This can be of the form "#AABBCC" or
      "rgb(255,100,200)" or "yellow". You can also specify null or "" to turn off
      fill.

      Type: string
      Default: #A7B1C4
  *)
  -> ?rangeSelectorPlotFillGradientColor:Color.t
  (** rangeSelectorPlotFillGradientColor http://dygraphs.com/options.html#rangeSelectorPlotFillGradientColor

      The top color for the range selector mini plot fill color gradient. This can
      be of the form "#AABBCC" or "rgb(255,100,200)" or "rgba(255,100,200,42)" or
      "yellow". You can also specify null or "" to disable the gradient and fill
      with one single color.

      Type: string
      Default: white
  *)
  -> ?rangeSelectorPlotLineWidth:float
  (** rangeSelectorPlotLineWidth http://dygraphs.com/options.html#rangeSelectorPlotLineWidth

      The width of the range selector mini plot line.

      Type: float
      Default: 1.5
  *)
  -> ?rangeSelectorPlotStrokeColor:Color.t
  (** rangeSelectorPlotStrokeColor http://dygraphs.com/options.html#rangeSelectorPlotStrokeColor

      The range selector mini plot stroke color. This can be of the form "#AABBCC"
      or "rgb(255,100,200)" or "yellow". You can also specify null or "" to turn
      off stroke.

      Type: string
      Default: #808FAB
  *)
  -> ?showRangeSelector:bool
  (** showRangeSelector http://dygraphs.com/options.html#showRangeSelector

      Show or hide the range selector widget.

      Type: boolean
      Default: false
  *)
  -> ?series:Series.t
  (** series http://dygraphs.com/options.html#series

      Defines per-series options. Its keys match the y-axis label names, and the values
      are dictionaries themselves that contain options specific to that series.

      Type: Object
      Default: null
  *)
  -> ?digitsAfterDecimal:int
  (** digitsAfterDecimal http://dygraphs.com/options.html#digitsAfterDecimal

      Unless it's run in scientific mode (see the sigFigs option), dygraphs
      displays numbers with digitsAfterDecimal digits after the decimal
      point. Trailing zeros are not displayed, so with a value of 2 you'll
      get '0', '0.1', '0.12', '123.45' but not '123.456' (it will be
      rounded to '123.46'). Numbers with absolute value less than
      0.1^digitsAfterDecimal (i.e. those which would show up as '0.00')
      will be displayed in scientific notation.

      Type: integer
      Default: 2
  *)
  -> ?labelsKMB:bool
  (** labelsKMB http://dygraphs.com/options.html#labelsKMB

      Show K/M/B for thousands/millions/billions on y-axis.

      Type: boolean
      Default: false
  *)
  -> ?labelsKMG2:bool
  (** labelsKMG2 http://dygraphs.com/options.html#labelsKMG2

      Show k/M/G for kilo/Mega/Giga on y-axis. This is different than labelsKMB in
      that it uses base 2, not 10.

      Type: boolean
      Default: false
  *)
  -> ?labelsUTC:bool
  (** labelsUTC http://dygraphs.com/options.html#labelsUTC

      Show date/time labels according to UTC (instead of local time).

      Type: boolean
      Default: false
  *)
  -> ?maxNumberWidth:int
  (** maxNumberWidth http://dygraphs.com/options.html#maxNumberWidth

      When displaying numbers in normal (not scientific) mode, large numbers will
      be displayed with many trailing zeros (e.g. 100000000 instead of 1e9). This
      can lead to unwieldy y-axis labels. If there are more than maxNumberWidth
      digits to the left of the decimal in a number, dygraphs will switch to
      scientific notation, even when not operating in scientific mode. If you'd
      like to see all those digits, set this to something large, like 20 or 30.

      Type: integer
      Default: 6
  *)
  -> ?sigFigs:int
  (** sigFigs http://dygraphs.com/options.html#sigFigs

      By default, dygraphs displays numbers with a fixed number of digits after the
      decimal point. If you'd prefer to have a fixed number of significant figures,
      set this option to that number of sig figs. A value of 2, for instance, would
      cause 1 to be display as 1.0 and 1234 to be displayed as 1.23e+3.

      Type: integer
      Default: null
  *)
  -> unit
  -> t
[@@js.builder           ]

[@@@ocamlformat "enable"]

val legendFormatter : t -> (Legend_data.t -> string) option [@@js.get]

val zoomCallback    : t -> (xmin:float -> xmax:float -> yRanges:Range.t array -> unit) option
[@@js.get]

(** This is the lodash.js deep-merge implementation *)
val merge_internal : t -> prefer:t -> t
[@@js.global "_.merge"]

(* [merge_internal] actually mutably changes the first [t] (and returns it) *)

(** merge two [t]s, preferring options in [prefer] *)
val merge : t -> prefer:t -> t
[@@js.custom
  let merge t ~prefer = create () |> merge_internal ~prefer:t |> merge_internal ~prefer]
