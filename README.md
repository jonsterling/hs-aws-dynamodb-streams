Haskell Bindings for [Amazon DynamoDb Streams](https://aws.amazon.com/dynamodb/)
=====================================================================

[Amazon DynamoDb Streams API Reference](http://dynamodb-preview.s3-website-us-west-2.amazonaws.com/docs/streams-api/Welcome.html)

Installation
============

Assuming that the Haskell compiler *GHC* and the Haskell build tool *cabal* is already
installed run the following command from the shell:

~~~{.sh}
cabal install --enable-documentation
~~~

Example Usage
=============

Here is a very simple example for making a call to DynamoDb Streams.
For more ellaborate usage refer to the [documentation of the AWS
package](https://hackage.haskell.org/package/aws).

~~~{.haskell}
import Aws
import Aws.General
import Aws.DynamoDb.Streams

main ∷ IO ()
main = do
  awsConfiguration ← baseConfiguration
  let configuration = StreamsConfiguration UsEast1
  ListStreamsResponse{..} ← simpleAws awsConfiguration configuration listStreams
  DescribeStreamResponse StreamDescription{..} ← simpleAws awsConfiguration configuration . describeStream $ head _lstrStreamIds

  GetShardIteratorResponse{..} ← simpleAws awsConfiguration configuration $
    let Just shardId = _shShardId $ head _sdShards
        Just streamId = _sdStreamId
    in getShardIterator streamId shardId ShardIteratorLatest

  resp ← simpleAws awsConfiguration configuration $ getRecords _gsirShardIterator
  print resp
~~~

