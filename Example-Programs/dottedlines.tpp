test(x) {
    fd x
}

dottedLines() {
    changeCol green;
    test(10);
    penUp;
    test(10);
    penDown;
    changeCol blue;
    moveTo 0 0;
    rt 90;
    test(10);
    penUp;
    test(10);
    changeCol green;
    penDown;
    test(10)
}

main() {
    dottedLines()
}
