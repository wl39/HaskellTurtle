test(x) {
    fd x
}

makeTriangle(x) {
    test(x);
    rt 120;
    test(x);
    rt 120;
    test(x)
}

makeSierpinkskiTriangle(x, y) {
    rt 30;
    makeTriangle(x);
    rt 120;
    test(x/2);
    rt 60;
    sierpinski(x/2, y)
}

sierpinski(x,y) {
    if (y > 0) then {
      repeat 3 {
        test(x/2);
        lt 120;
        sierpinski(x/2, y-1);
        test(x/2);
        rt 120
      }
    } else {};
    rt 120
}

main() {
    makeSierpinkskiTriangle(300, 10)
}
