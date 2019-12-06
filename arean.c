#include<stdio.h>
#include<math.h>
#include<stdio.h>
int main(){
    int i,j,n;
    float a[10][2],area=0;
    
    printf("Enter sides of Polygon\n");
    scanf("%d",&n);
    
    if (n < 3)
       printf ("Area is : %f",area);
    else {
       for (i=0; i < n; i++){
          printf("Enter (x%d , y%d)  :",(i+1),(i+1));
          for (j=0; i < 2; j++){
            scanf("%f",&a[i][j]);
          }
          printf("\n");
       }
    
    for (i=0; i < n; i++){
       printf("(x%d , y%d)  :",i,i);
          for (j=0; i < 2; j++){
             printf ("%f",a[i][j]);
         }
    }}
}
