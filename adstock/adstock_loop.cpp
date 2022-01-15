cppFunction('NumericVector adstock_loop(double dcy, NumericVector x, int arr_size) {
  int i;
  int n = arr_size;
  NumericVector y(arr_size);

   for( int i = 0; i < n; ++i ) {
           if (i==1 ){
                y[i]=x[i];
    }

    else {
                y[i]=x[i]+y[i-1]*(1-dcy);
        
    }
   }
  return y;
}')


// result = adstock_loop(x, datamax$spk, 100)
