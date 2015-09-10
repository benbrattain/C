//
//  main.c
//  Lecture 4
//
//  Created by Benjamin Brattain on 9/2/15.
//  Copyright (c) 2015 Benjamin Brattain. All rights reserved.
//


#include <stdio.h>
int main(void) {
    int number, temp, result = 0;
    scanf("%d", &number);
    temp = number;
    while(temp >=1){
        result = result + (temp%10);
        temp = temp/10;
    }
    
    printf("The sum of the digits of given number %d",result);
}