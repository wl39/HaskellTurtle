test(x) {
    fd x
}

square(x) {
    test(x);
    rt 90;
    test(x);
    rt 90;
    test(x);
    rt 90;
    test(x);
    rt 90
}

octagon(x) {
    test(x);
    rt 45;
    test(x);
    rt 45;
    test(x);
    rt 45;
    test(x);
    rt 45;
    test(x);
    rt 45;
    test(x);
    rt 45;
    test(x);
    rt 45;
    test(x);
    rt 45
}

maybeStar(x) {
    rt 90;
    test(x);
    rt 144;
    test(x);
    rt 144;
    test(x);
    rt 144;
    test(x);
    rt 144;
    test(x);
    rt 144
}

scottishFlag() {
    test(150);
    rt 90;
    test(250);
    rt 90;
    test(150);
    rt 90;
    test(250);
    rt 90;
    test(25);
    rt 59;
    test(97.3);
    lt 118;
    test(97.3);
    rt 59.04;
    test(25);
    rt 90;
    test(25);
    rt 30.96;
    test(116.5);
    lt 61.92;
    test(116.5);
    rt 30.96;
    test(25);
    rt 90;
    test(25);
    rt 59.04;
    test(97.3);
    lt 118.08;
    test(97.3);
    rt 59.04;
    test(25);
    rt 90;
    test(25);
    rt 30.96;
    test(116.5);
    lt 61.92;
    test(116.5);
    rt 31
}

makeTriangle(x) {
    test(x);
    rt 120;
    test(x);
    rt 120;
    test(x)
}

testSierpinski(x){
    test(x/2);
    lt 120;
    makeTriangle(x/2);
    lt 120;
    test(x/2);
    rt 120;
    test(x/2);
    lt 120;
    makeTriangle(x/2);
    lt 120;
    test(x/2);
    rt 120;
    test(x/2);
    lt 120;
    makeTriangle(x/2);
    lt 120;
    test(x/2);
    lt 120

}


sierpinkskiTriangle(x, y) {
    test(x/2);
    lt 120;
    testSierpinski(x/2);
    test(x/2);
    rt 120;
    test(x/2);
    lt 120;
    testSierpinski(x/2);
    test(x/2);
    rt 120;
    test(x/2);
    lt 120;
    testSierpinski(x/2);
    test(x/2);
    lt 120
}

makeSierpinkskiTriangle(x, y) {
    rt 30;
    makeTriangle(x);
    rt 120;
    test(x/2);
    rt 60;
    sierpinkskiTriangle(x/2, y)
}







main() {
makeSierpinkskiTriangle(200,2)
}
