interface A<T>
{
   x : A<()=>T>
}
 
interface B<T>
{
   x : B<()=>T>
}
 
var a: A<string>
var b: B<string> = a
