const re = /./g
let match: RegExpExecArray | null

while ((match = re.exec("xxx")) != null) {
    const length = match[1].length + match[2].length
}

export { }