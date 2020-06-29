module MS = Belt.Map.String;

type direction =
  | UpDown
  | DownUp
  | LeftRight
  | RightLeft;

let directionConstraints = direction =>
  SetCoLa.(
    switch (direction) {
    | DownUp => [|
        C({
          "name": "layer",
          "sets": {
            "partition": "depth",
          },
          "forEach": [|{"constraint": "align", "axis": "x"}|],
        }),
        C({
          "name": "sort",
          "sets": [|"layer"|],
          "forEach": [|{"constraint": "order", "axis": "y", "by": "depth"}|],
        }),
      |]
    | UpDown => [|
        C({
          "name": "layer",
          "sets": {
            "partition": "depth",
          },
          "forEach": [|{"constraint": "align", "axis": "x"}|],
        }),
        C({
          "name": "sort",
          "sets": [|"layer"|],
          "forEach": [|{"constraint": "order", "axis": "y", "by": "depth", "reverse": true}|],
        }),
      |]
    | RightLeft => [|
        C({
          "name": "layer",
          "sets": {
            "partition": "depth",
          },
          "forEach": [|{"constraint": "align", "axis": "y"}|],
        }),
        C({
          "name": "sort",
          "sets": [|"layer"|],
          "forEach": [|{"constraint": "order", "axis": "x", "by": "depth"}|],
        }),
      |]
    | LeftRight => [|
        C({
          "name": "layer",
          "sets": {
            "partition": "depth",
          },
          "forEach": [|{"constraint": "align", "axis": "y"}|],
        }),
        C({
          "name": "sort",
          "sets": [|"layer"|],
          "forEach": [|{"constraint": "order", "axis": "x", "by": "depth", "reverse": true}|],
        }),
      |]
    }
  );

let makeLinks = (linkRender, uids) => {
  // Js.log2("seq uids", uids |> Array.of_list);
  let stPairs = List.combine(List.rev(uids) |> List.tl |> List.rev, List.tl(uids));
  // Js.log2("pairs", stPairs |> Array.of_list);
  List.map(((source, target)): Link.t => Link.{source, target, linkRender}, stPairs);
};

/* NOTE: gap is between neighboring sides of bounding boxes */
/* TODO: need to recenter DownUp and RightLeft so they are contained in the positive quadrant.
   Maybe more reason to have layout take care of that type of stuff. */
/* TODO: add an alignment flag for beginning/middle/end or something */
let mk = (~uid=?, ~tag=?, ~nodes, ~linkRender, ~gap, ~direction) =>
  KernelIR.mk(
    ~uid?,
    ~tag?,
    ~nodes,
    ~links=makeLinks(linkRender, List.map((KernelIR.{uid}) => uid, nodes)),
    ~layout=
      (bboxes, _) => {
        let (uids, [n, ...rest] as ns) = List.split(bboxes);
        /* LR
            {w0, h0}
            {w1, h1}
            {w2, h2}
            -->
            {0, 0, w0, h0}
            {w0 + gap, 0, w1, h1}
            {(w0 + gap) + w1 + gap, 0, w2, h2}
           */
        let newBBox = ((transform, prevBBox), bbox) => {
          let width = bbox->Rectangle.width;
          let height = bbox->Rectangle.height;
          switch (direction) {
          | UpDown => (
              Transform.{
                scale: {
                  x: 1.,
                  y: 1.,
                },
                translate: {
                  x: -. bbox->Rectangle.x1 /* -. sizeOffset->Rectangle.width   /. 2. /* center horizontally */ */, /* move to origin */
                  y:
                    -. bbox->Rectangle.y1  /* move to origin */
                    +. transform.translate.y  /* translate past prev. bbox */
                    +. prevBBox->Rectangle.y2  /* translate past prev. bbox */
                    +. gap /* add gap */,
                },
              },
              bbox,
            )
          /* Rectangle.fromPointSize(~x=0., ~y=bbox->Rectangle.y2 +. gap, ~width, ~height); */
          | DownUp => raise(failwith("TODO"))
          /* Rectangle.fromCenterPointSize(
               ~cx=bbox->Rectangle.cx,
               ~cy=bbox->Rectangle.y1 -. gap -. height /. 2.,
               ~width=bbox->Rectangle.width,
               ~height=bbox->Rectangle.height,
             ); */
          | LeftRight => (
              Transform.{
                scale: {
                  x: 1.,
                  y: 1.,
                },
                translate: {
                  x:
                    -. bbox->Rectangle.x1  /* move to origin */
                    +. transform.translate.x  /* translate past prev. bbox */
                    +. prevBBox->Rectangle.x2  /* translate past prev. bbox */
                    +. gap /* add gap */,
                  y:
                    -. bbox->Rectangle.y1  /* move to origin */
                    -. bbox->Rectangle.height
                    /. 2. /* center vertically */,
                },
              },
              bbox,
            )
          /* Rectangle.fromPointSize(
               ~x=bbox->Rectangle.x2 +. gap,
               ~y=sizeOffset->Rectangle.y1,
               ~width,
               ~height,
             ) */
          | RightLeft => raise(failwith("TODO"))
          /* Rectangle.fromCenterPointSize(
               ~cx=bbox->Rectangle.x1 -. gap -. width /. 2.,
               ~cy=bbox->Rectangle.cy,
               ~width=bbox->Rectangle.width,
               ~height=bbox->Rectangle.height,
             ); */
          };
        };
        let initialBBox =
          switch (direction) {
          | UpDown => (
              Transform.{
                scale: {
                  x: 1.,
                  y: 1.,
                },
                translate: {
                  x: -. n->Rectangle.x1 /* -. n->Rectangle.width /. 2. */,
                  y: 0.,
                },
              },
              n,
            )
          | DownUp => raise(failwith("TODO"))
          | LeftRight => (
              {
                scale: {
                  x: 1.,
                  y: 1.,
                },
                translate: {
                  x: 0.,
                  y: -. n->Rectangle.y1 -. n->Rectangle.height /. 2.,
                },
              },
              n,
            )
          | RightLeft => raise(failwith("TODO"))
          };
        let (transforms, _) = Fn.scanl(newBBox, initialBBox, rest) |> List.split;
        // Js.log2("seq boxes", bboxes |> Array.of_list);
        // Js.log2("seq union", Rectangle.union_list(bboxes));
        /* Js.log2(
             "seq scanl boxes",
             Util.scanl((bbox, size) => newBBox(bbox, size), Node.sizeToBBox(n), rest)
             |> Array.of_list,
           );
           Js.log2("seq boxes", bboxes |> Array.of_list);
           Js.log2("seq boxes size", bboxes |> Array.of_list |> Array.map(Node.bboxToSize)); */
        /* Js.log2(
             "test",
             Util.scanl((a, b) => a / b, 64, [4, 2, 4]) |> Array.of_list,
           ); */
        // Js.log2("seq bboxes", bboxes |> Array.of_list);
        // Js.log2("[seq] transforms", transforms |> Array.of_list);
        List.combine(uids, transforms) |> Array.of_list |> MS.fromArray;
      },
    // Js.log2("seq ns sizes", ns |> Array.of_list);
    ~computeBBox=bs => bs->MS.valuesToArray->Array.to_list->Rectangle.union_list,
    ~nodeRender=_ => React.null,
  );