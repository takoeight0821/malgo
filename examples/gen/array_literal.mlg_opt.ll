; ModuleID = './examples/gen/array_literal.mlg.ll'
source_filename = "./examples/array_literal.mlg"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

declare {}* @print_int(i64) local_unnamed_addr

define {}* @print_int0(i64) local_unnamed_addr {
  %2 = tail call {}* @print_int(i64 %0)
  ret {}* %2
}

declare {}* @newline() local_unnamed_addr

define {}* @newline1() local_unnamed_addr {
  %1 = tail call {}* @newline()
  ret {}* %1
}

declare void @GC_init() local_unnamed_addr

declare i8* @GC_malloc(i64) local_unnamed_addr

define i32 @main() local_unnamed_addr {
body_0:
  tail call void @GC_init()
  %0 = tail call i8* @GC_malloc(i64 80)
  %1 = tail call i8* @GC_malloc(i64 16)
  %2 = bitcast i8* %1 to i64**
  %3 = bitcast i8* %1 to i8**
  store i8* %0, i8** %3, align 8
  %4 = getelementptr i8, i8* %1, i64 8
  %5 = bitcast i8* %4 to i64*
  store i64 10, i64* %5, align 8
  %.cast = bitcast i8* %0 to i64*
  store i64 1, i64* %.cast, align 8
  %6 = load i64*, i64** %2, align 8
  %7 = getelementptr i64, i64* %6, i64 1
  store i64 1, i64* %7, align 8
  %8 = load i64*, i64** %2, align 8
  %9 = getelementptr i64, i64* %8, i64 2
  store i64 1, i64* %9, align 8
  %10 = load i64*, i64** %2, align 8
  %11 = getelementptr i64, i64* %10, i64 3
  store i64 1, i64* %11, align 8
  %12 = load i64*, i64** %2, align 8
  %13 = getelementptr i64, i64* %12, i64 4
  store i64 1, i64* %13, align 8
  %14 = load i64*, i64** %2, align 8
  %15 = getelementptr i64, i64* %14, i64 5
  store i64 1, i64* %15, align 8
  %16 = load i64*, i64** %2, align 8
  %17 = getelementptr i64, i64* %16, i64 6
  store i64 1, i64* %17, align 8
  %18 = load i64*, i64** %2, align 8
  %19 = getelementptr i64, i64* %18, i64 7
  store i64 1, i64* %19, align 8
  %20 = load i64*, i64** %2, align 8
  %21 = getelementptr i64, i64* %20, i64 8
  store i64 1, i64* %21, align 8
  %22 = load i64*, i64** %2, align 8
  %23 = getelementptr i64, i64* %22, i64 9
  store i64 1, i64* %23, align 8
  %24 = load i64*, i64** %2, align 8
  %25 = getelementptr i64, i64* %24, i64 1
  store i64 2, i64* %25, align 8
  %26 = load i64*, i64** %2, align 8
  %27 = getelementptr i64, i64* %26, i64 2
  store i64 3, i64* %27, align 8
  %28 = load i64*, i64** %2, align 8
  %29 = getelementptr i64, i64* %28, i64 3
  store i64 4, i64* %29, align 8
  %30 = load i64*, i64** %2, align 8
  %31 = getelementptr i64, i64* %30, i64 4
  store i64 5, i64* %31, align 8
  %32 = load i64*, i64** %2, align 8
  %33 = getelementptr i64, i64* %32, i64 5
  store i64 6, i64* %33, align 8
  %34 = load i64*, i64** %2, align 8
  %35 = getelementptr i64, i64* %34, i64 6
  store i64 7, i64* %35, align 8
  %36 = load i64*, i64** %2, align 8
  %37 = getelementptr i64, i64* %36, i64 7
  store i64 8, i64* %37, align 8
  %38 = load i64*, i64** %2, align 8
  %39 = getelementptr i64, i64* %38, i64 8
  store i64 9, i64* %39, align 8
  %40 = load i64*, i64** %2, align 8
  %41 = getelementptr i64, i64* %40, i64 9
  store i64 10, i64* %41, align 8
  %42 = load i64*, i64** %2, align 8
  %43 = getelementptr i64, i64* %42, i64 1
  %44 = load i64, i64* %43, align 8
  %45 = tail call {}* @print_int(i64 %44)
  %46 = tail call {}* @newline()
  %47 = load i64*, i64** %2, align 8
  %48 = getelementptr i64, i64* %47, i64 2
  %49 = load i64, i64* %48, align 8
  %50 = tail call {}* @print_int(i64 %49)
  %51 = tail call {}* @newline()
  ret i32 0
}
