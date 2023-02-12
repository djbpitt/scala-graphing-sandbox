import smile.feature.transform.WinsorScaler
import smile.data.DataFrame
/*
* Roll your own StandardScaler (= we are not doing this)
* https://sebastianraschka.com/Articles/2014_about_feature_scaling.html#standardization-and-min-max-scaling
# Standardization

x = [1,4,5,6,6,2,3]
mean = sum(x)/len(x)
std_dev = (1/len(x) * sum([ (x_i - mean)**2 for x_i in x]))**0.5

z_scores = [(x_i - mean)/std_dev for x_i in x]
*/

// https://stackoverflow.com/questions/64382334/smile-scala-api-create-dataframe-from-array/64411258#64411258
@main def scaling_practice: Unit =
  val data = Array(
    Array(1.0, 2.0),
    Array(4.0, 3.0),
    Array(12.0, 14.0)
  )
  val df = DataFrame.of(data)
  println(df)
  val scaler = WinsorScaler.fit(df, 0.01, 0.99)
  print(scaler)
  val transformed = scaler.apply(df)
  print(transformed)
