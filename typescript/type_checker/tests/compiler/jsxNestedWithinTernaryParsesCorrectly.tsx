// @jsx: preserve
const emptyMessage = null as any;
const a = (
    <div>
      {0 ? (
        emptyMessage // must be identifier?
      ) : (
          // must be exactly two expression holes
        <span>
          {0}{0}
        </span>
      )}
    </div>
);