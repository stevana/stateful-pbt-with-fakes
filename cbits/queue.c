#include <stdlib.h>
#include "queue.h"

Queue *new(int n) {
  int *buff = malloc((n + 1)*sizeof(int));
  Queue q = {buff,0,0,n + 1};
  Queue *qptr = malloc(sizeof(Queue));
  *qptr = q;
  return qptr;
}

Queue *newBroken(int n) {
  int *buff = malloc(n*sizeof(int));
  Queue q = {buff,0,0,n};
  Queue *qptr = malloc(sizeof(Queue));
  *qptr = q;
  return qptr;
}

void put(Queue *q, int n) {
  q->buf[q->inp] = n;
  q->inp = (q->inp + 1) % q->size;
}

int get(Queue *q) {
  int ans = q->buf[q->outp];
  q->outp = (q->outp + 1) % q->size;
  return ans;
}

int size(Queue *q) {
  return (q->inp - q->outp + q->size) % q->size;
}

int sizeBroken(Queue *q) {
  return (q->inp - q->outp) % q->size;
}

int sizeBroken2(Queue *q) {
  return abs(q->inp - q->outp) % q->size;
}
