; ModuleID = './examples/lambda.mlg'
source_filename = "./examples/lambda.mlg"

declare i8* @GC_malloc(i64)

define { i64 (i8*, i64)*, i8* }* @"$lambda.31"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %6 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda.31", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %6
  %7 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = call i8* @GC_malloc(i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64))
  %9 = bitcast i8* %8 to { i64 }*
  %10 = getelementptr { i64 }, { i64 }* %9, i32 0, i32 0
  store i64 %1, i64* %10
  %11 = bitcast { i64 }* %9 to i8*
  %12 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %13 = bitcast i8* %12 to { i64 (i8*, i64)*, i8* }*
  %14 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.32", i64 (i8*, i64)** %14
  %15 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %13, i32 0, i32 1
  store i8* %11, i8** %15
  ret { i64 (i8*, i64)*, i8* }* %13
}

define i64 @"$lambda.32"(i8*, i64) {
  %3 = bitcast i8* %0 to { i64 }*
  %4 = getelementptr { i64 }, { i64 }* %3, i32 0, i32 0
  %5 = load i64, i64* %4
  %6 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %7 = bitcast i8* %6 to { i64 (i8*, i64)*, i8* }*
  %8 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.32", i64 (i8*, i64)** %8
  %9 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %7, i32 0, i32 1
  store i8* %0, i8** %9
  %10 = add i64 %5, %1
  ret i64 %10
}

define i64 @"$lambda.29"(i8*, i64) {
  %3 = bitcast i8* %0 to {}*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.29", i64 (i8*, i64)** %6
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %0, i8** %7
  %8 = add i64 %1, 1
  ret i64 %8
}

declare {}* @newline({}*)

define {}* @newline.10({}*) {
  %2 = call {}* @newline({}* %0)
  ret {}* %2
}

declare {}* @print_int(i64)

define {}* @print_int.9(i64) {
  %2 = call {}* @print_int(i64 %0)
  ret {}* %2
}

define i32 @main() {
  %1 = call i8* @GC_malloc(i64 0)
  %2 = bitcast i8* %1 to {}*
  %3 = bitcast {}* %2 to i8*
  %4 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %5 = bitcast i8* %4 to { i64 (i8*, i64)*, i8* }*
  %6 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  store i64 (i8*, i64)* @"$lambda.29", i64 (i8*, i64)** %6
  %7 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  store i8* %3, i8** %7
  %8 = call i8* @GC_malloc(i64 0)
  %9 = bitcast i8* %8 to {}*
  %10 = bitcast {}* %9 to i8*
  %11 = call i8* @GC_malloc(i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2))
  %12 = bitcast i8* %11 to { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }*
  %13 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %12, i32 0, i32 0
  store { i64 (i8*, i64)*, i8* }* (i8*, i64)* @"$lambda.31", { i64 (i8*, i64)*, i8* }* (i8*, i64)** %13
  %14 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %12, i32 0, i32 1
  store i8* %10, i8** %14
  %15 = alloca i64
  br i1 true, label %then_0, label %else_0

then_0:                                           ; preds = %0
  store i64 42, i64* %15
  br label %endif_0

else_0:                                           ; preds = %0
  store i64 48, i64* %15
  br label %endif_0

endif_0:                                          ; preds = %else_0, %then_0
  %16 = load i64, i64* %15
  %17 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 0
  %18 = load i64 (i8*, i64)*, i64 (i8*, i64)** %17
  %19 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %5, i32 0, i32 1
  %20 = load i8*, i8** %19
  %21 = call i64 %18(i8* %20, i64 41)
  %22 = call {}* @print_int.9(i64 %21)
  %23 = call i8* @GC_malloc(i64 0)
  %24 = bitcast i8* %23 to {}*
  %25 = call {}* @newline.10({}* %24)
  %26 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %12, i32 0, i32 0
  %27 = load { i64 (i8*, i64)*, i8* }* (i8*, i64)*, { i64 (i8*, i64)*, i8* }* (i8*, i64)** %26
  %28 = getelementptr { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }, { { i64 (i8*, i64)*, i8* }* (i8*, i64)*, i8* }* %12, i32 0, i32 1
  %29 = load i8*, i8** %28
  %30 = call { i64 (i8*, i64)*, i8* }* %27(i8* %29, i64 2)
  %31 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %30, i32 0, i32 0
  %32 = load i64 (i8*, i64)*, i64 (i8*, i64)** %31
  %33 = getelementptr { i64 (i8*, i64)*, i8* }, { i64 (i8*, i64)*, i8* }* %30, i32 0, i32 1
  %34 = load i8*, i8** %33
  %35 = call i64 %32(i8* %34, i64 3)
  %36 = call {}* @print_int.9(i64 %35)
  %37 = call i8* @GC_malloc(i64 0)
  %38 = bitcast i8* %37 to {}*
  %39 = call {}* @newline.10({}* %38)
  ret i32 0
}