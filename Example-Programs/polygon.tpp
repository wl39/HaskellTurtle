test(x) {
    fd x
}

polygon(x, n) {
    a = 360/n;
    rt a;
    counter = 0;
    while (counter < n) {
        test(x);
        rt a;
        counter = counter + 1
    }
    
}

main() {
    polygon(100, 6)
}
