//@strict: true

type ReducerArgs =
  | ["add", { a: number; b: number }]
  | ["concat", { firstArr: any[]; secondArr: any[] }];

const reducer: (...args: ReducerArgs) => void = (op, args) => {
  switch (op) {
    case "add":
      console.log(args.a + args.b);
      break;
    case "concat":
      console.log(args.firstArr.concat(args.secondArr));
      break;
  }
};
