module MS = Belt.Map.String;

let makeTableLinks = (xLinkRender, yLinkRender, uids) => {
  // Js.log2("table uids", uids |> List.map(Array.of_list) |> Array.of_list);
  let horizontalPairs = List.map(Seq.makeLinks(xLinkRender), uids |> Matrix.toListList);
  // Js.log2("hPairs", horizontalPairs |> List.map(Array.of_list) |> Array.of_list);
  let verticalPairs =
    List.map(Seq.makeLinks(yLinkRender), Matrix.transpose(uids) |> Matrix.toListList);
  // Js.log2("vPairs", verticalPairs |> List.map(Array.of_list) |> Array.of_list);
  List.flatten(horizontalPairs @ verticalPairs);
};

/* table. Like a nested hSeq and vSeq except allows layout and rendering to be influenced by all
   elements of the table */
/* kind of weird, because rows seem like a natural linkage more than columns, but need to consider
   them equally. hopefully this doesn't mess with transformations too much */
/* nodes is a list of rows */
/* TODO: add more customization for row and column arrangement */
let mk =
    (
      ~uid=?,
      ~tag=?,
      ~nodes,
      ~xLinkRender,
      ~yLinkRender,
      ~xGap,
      ~yGap,
      ~xDirection,
      ~yDirection,
      (),
    ) => {
  let colLen = List.length(nodes);
  let rowLen = List.length(List.nth(nodes, 0));
  KernelIR.mk(
    ~uid?,
    ~tag?,
    ~nodes=List.flatten(nodes),
    ~links=
      makeTableLinks(
        xLinkRender,
        yLinkRender,
        nodes |> Matrix.fromListList |> Matrix.map((KernelIR.{uid}) => uid),
      ),
    ~layout=
      (bboxes, _) => {
        let (uids, ns) = List.split(bboxes);
        /* reconstruct matrix */
        let mat = Matrix.fromList(ns, rowLen);

        /* max height per row */
        let maxHeightPerRow: list(float) =
          mat
          |> Matrix.toListList
          |> List.map(row => row |> List.map(Rectangle.height) |> List.fold_left(max, 0.));
        let cumulativeHeight = Fn.scanl((+.), 0., maxHeightPerRow);
        /* transpose */
        let matT = Matrix.transpose(mat);
        /* max width per col */
        let maxWidthPerCol: list(float) =
          matT
          |> Matrix.toListList
          |> List.map(col => col |> List.map(Rectangle.width) |> List.fold_left(max, 0.));
        let cumulativeWidth = Fn.scanl((+.), 0., maxWidthPerCol);

        /* TODO: incorporate gap and direction info. currently assumes gaps are 0 and directions are
           topleft to bottomright */
        let computeTransform = (i, j, bbox) => {
          let widthSoFar = List.nth(cumulativeWidth, j);
          let heightSoFar = List.nth(cumulativeHeight, i);
          let cellWidth = List.nth(maxWidthPerCol, j);
          let cellHeight = List.nth(maxHeightPerRow, i);
          let widthPadding = cellWidth -. bbox->Rectangle.width;
          let heightPadding = cellHeight -. bbox->Rectangle.height;
          Transform.{
            scale: {
              x: 1.,
              y: 1.,
            },
            translate: {
              x: -. bbox->Rectangle.x1 +. widthPadding /. 2. +. widthSoFar,
              y: -. bbox->Rectangle.y1 +. heightPadding /. 2. +. heightSoFar,
            },
          };
        };

        let transforms =
          mat
          |> Matrix.toListList
          |> List.mapi((i, row) => row |> List.mapi((j, so) => computeTransform(i, j, so)))
          |> List.flatten;
        // Js.log2("table transforms", transforms |> Array.of_list);
        List.combine(uids, transforms) |> Array.of_list |> MS.fromArray;
      },
    ~computeBBox=bs => bs->MS.valuesToArray->Array.to_list->Rectangle.union_list,
    ~nodeRender=_ => React.null,
  );
};