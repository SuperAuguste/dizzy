# dizzy WIP

Zig diffing from the ground up with Myers' algorithm for discovering shortest edit scripts.

## Mental Model for Myers Diff

Draw a diagonal line on matrix. As depth D increases, draw D diagonal lines on each depth iteration. Always move towards the line. Prioritize rightward movement for deletions, and downward for insertions.

## Resources

- https://www.nathaniel.ai/myers-diff/
- https://blog.robertelder.org/diff-algorithm/
- https://blog.jcoglan.com/2017/02/15/the-myers-diff-algorithm-part-2/

## Prior Art

- https://github.com/ziglibs/diffz

## License

MIT
