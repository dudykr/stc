// Modified repro from #12573

type Handlers<T> = { [K in keyof T]: (t: T[K]) => void }

declare function on<T>(handlerHash: Handlers<T>): T

var hashOfEmpty1 = on({
    test: () => {
    }
});  // {}
