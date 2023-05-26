test(x) {
    fd x
}

polygonSpiral(p, n) {
    a = 360/p;
    lt a;
    for (i = 1; i<n+1; i=i+1) {
      test(i);
      rt a
    }
}

polygonSpiral2(p, n) {
    a = 360/p;
    lt a;
    i = 1;
    for (; i<n+1; i=i+1) {
      test(i);
      rt a
    }
}

polygonSpiral3(p, n) {
    a = 360/p;
    lt a;
    i = 1;
    for (; i<n+1; ) {
      test(i);
      rt a;
      i = i + 1
    }
}

main() {
    polygonSpiral(5, 100);
    polygonSpiral2(5,100);
    polygonSpiral3(5,100);
}
