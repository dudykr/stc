const a = {
  get x() {
    return "boolean" as const;
  },
  y: "string",
};
