; ModuleID = './examples/lambda.mlg'
source_filename = "./examples/lambda.mlg"


 


declare external ccc  {}* @print_int(i64)    


define external ccc  {}* @print_int0(i64 )    {
  %2 =  call ccc  {}*  @print_int(i64  %0)  
  ret {}* %2 
}


declare external ccc  {}* @newline()    


define external ccc  {}* @newline1()    {
  %1 =  call ccc  {}*  @newline()  
  ret {}* %1 
}


declare external ccc  i8* @GC_malloc(i64)    


define external ccc  {i64 (i8*, i64)*, i8*}* @$lambda25(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* 
  %6 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}* (i8*, i64)* @$lambda25, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %6 
  %7 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64}* getelementptr inbounds ({i64}, {i64}* inttoptr (i32 0 to {i64}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i64}* 
  %10 = getelementptr  {i64}, {i64}* %9, i32 0, i32 0 
  store  i64 %1, i64* %10 
  %11 = bitcast {i64}* %9 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %13 = bitcast i8* %12 to {i64 (i8*, i64)*, i8*}* 
  %14 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %13, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda24, i64 (i8*, i64)** %14 
  %15 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %13, i32 0, i32 1 
  store  i8* %11, i8** %15 
  ret {i64 (i8*, i64)*, i8*}* %13 
}


define external ccc  i64 @$lambda24(i8* , i64 )    {
  %3 = bitcast i8* %0 to {i64}* 
  %4 = getelementptr  {i64}, {i64}* %3, i32 0, i32 0 
  %5 = load  i64, i64* %4 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i64 (i8*, i64)*, i8*}* 
  %8 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda24, i64 (i8*, i64)** %8 
  %9 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %7, i32 0, i32 1 
  store  i8* %0, i8** %9 
  %10 = add   i64 %5, %1 
  ret i64 %10 
}


define external ccc  i64 @$lambda23(i8* , i64 )    {
  %3 = bitcast i8* %0 to {}* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64 (i8*, i64)*, i8*}* 
  %6 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda23, i64 (i8*, i64)** %6 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %0, i8** %7 
  %8 = add   i64 %1, 1 
  ret i64 %8 
}


declare external ccc  void @GC_init()    


define external ccc  i32 @main()    {
; <label>:0:
   call ccc  void  @GC_init()  
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {}* 
  %3 = bitcast {}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64 (i8*, i64)*, i8*}* getelementptr inbounds ({i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* inttoptr (i32 0 to {i64 (i8*, i64)*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i64 (i8*, i64)*, i8*}* 
  %6 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  store  i64 (i8*, i64)* @$lambda23, i64 (i8*, i64)** %6 
  %7 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  store  i8* %3, i8** %7 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {}* 
  %10 = bitcast {}* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* getelementptr inbounds ({{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* inttoptr (i32 0 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* 
  %13 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %12, i32 0, i32 0 
  store  {i64 (i8*, i64)*, i8*}* (i8*, i64)* @$lambda25, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %13 
  %14 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %12, i32 0, i32 1 
  store  i8* %10, i8** %14 
  %15 = alloca i64 
  br i1 1, label %then_0, label %else_0 
then_0:
  store  i64 42, i64* %15 
  br label %end_0 
else_0:
  store  i64 48, i64* %15 
  br label %end_0 
end_0:
  %16 = load  i64, i64* %15 
  %17 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 0 
  %18 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %17 
  %19 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %5, i32 0, i32 1 
  %20 = load  i8*, i8** %19 
  %21 =  call ccc  i64  %18(i8*  %20, i64  41)  
  %22 =  call ccc  {}*  @print_int0(i64  %21)  
  %23 =  call ccc  {}*  @newline1()  
  %24 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %12, i32 0, i32 0 
  %25 = load  {i64 (i8*, i64)*, i8*}* (i8*, i64)*, {i64 (i8*, i64)*, i8*}* (i8*, i64)** %24 
  %26 = getelementptr  {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}, {{i64 (i8*, i64)*, i8*}* (i8*, i64)*, i8*}* %12, i32 0, i32 1 
  %27 = load  i8*, i8** %26 
  %28 =  call ccc  {i64 (i8*, i64)*, i8*}*  %25(i8*  %27, i64  2)  
  %29 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %28, i32 0, i32 0 
  %30 = load  i64 (i8*, i64)*, i64 (i8*, i64)** %29 
  %31 = getelementptr  {i64 (i8*, i64)*, i8*}, {i64 (i8*, i64)*, i8*}* %28, i32 0, i32 1 
  %32 = load  i8*, i8** %31 
  %33 =  call ccc  i64  %30(i8*  %32, i64  3)  
  %34 =  call ccc  {}*  @print_int0(i64  %33)  
  %35 =  call ccc  {}*  @newline1()  
  ret i32 0 
}
