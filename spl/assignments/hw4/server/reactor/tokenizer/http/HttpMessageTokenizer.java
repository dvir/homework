package tokenizer.http;

import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CharacterCodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import tokenizer.MessageTokenizer;

public class HttpMessageTokenizer implements MessageTokenizer<HttpMessage> {
	private final Vector<ByteBuffer> _messageBuf = new Vector<ByteBuffer>();
	private final Vector<HttpRequestMessage> _messages = new Vector<HttpRequestMessage>();
	private final CharsetDecoder _ascii_decoder;
	private final CharsetEncoder _ascii_encoder;

	public HttpMessageTokenizer() {
		Charset charset = Charset.forName("US-ASCII"); //HTTP encoding is US-ASCII (only the body can be in different encoding) 
		_ascii_decoder=charset.newDecoder();
		_ascii_encoder=charset.newEncoder();
	}

	@Override
	public synchronized void addBytes(ByteBuffer bytes) {
		this._messageBuf.add(bytes);
	}

	/**
	 * A helper function for finding the first line in a {@link StringBuffer} and removing it.
	 * A line ends with <b>\r\n</b> characters.
	 * @param sb The {@link StringBuffer}
	 * @return The first line in sb
	 */
	private static String getLineFromStringBuffer(StringBuffer sb){
		int newline=sb.indexOf("\r\n");
		String line=sb.substring(0,newline);
		sb.delete(0, newline+2);
		return line;
	}

	/**
	 * This function removes the first {@link HttpMessage} inside <b>_messageBuf</b> and insert it into <b>_messages</b>.<br/>
	 * If <b>_messageBuf</b> does not contain a complete message, then the function return <b>_messageBuf</b> to its original state (The position and limit markers are set back to their original position).  
	 */
	private synchronized void proccessData(){
		HttpOperator operator=null;
		String URI=null;
		String protocol=null;
		Map<String, List<String>> headers=new Hashtable<String, List<String>>();
		ByteBuffer body=null;
		int ContentLength=0;
		StringBuffer sb=new StringBuffer();

		for (int i=0; i<_messageBuf.size(); i++) {  //In this loop we will try to get the beginning of the first HTTP message (up to \r\n\r\n)
			ByteBuffer bytes = _messageBuf.get(i);
			CharBuffer chars = CharBuffer.allocate(bytes.remaining());
			this._ascii_decoder.decode(bytes, chars, false); // false: more bytes may follow. Any unused bytes are kept in the decoder.
			chars.flip();
			StringBuffer tmp=new StringBuffer(chars);
			sb.append(tmp);
			int index;
			if((index=tmp.indexOf("\r\n\r\n"))!=-1){ // index now holds the beginning of the body in the current ByteBuffer (_messageBuf.get(i))
				bytes.position(index+4); // Move the current ByteBuffer position to the beginning of the HTTP body so we will be able to retrieve it later.

				//get initial request line
				String line=getLineFromStringBuffer(sb);
				String temp[]=line.split(" ");
				operator=HttpOperator.valueOf(temp[0]);
				URI=temp[1];
				protocol=temp[2];

				//get headers
				while(!(line=getLineFromStringBuffer(sb)).equals(""))
					HttpMessage.addHeader(headers, line);
				
				//update content-length
				List<String> values= headers.get("content-length");
				if(values!=null && values.size()>0)
					ContentLength=Integer.decode(values.get(0).trim());

				// get body. Body can be divided into more than one ByteBuffer. 
				body=ByteBuffer.allocate(ContentLength);
				while(body.remaining()>0 && i<_messageBuf.size()){
					bytes = _messageBuf.get(i);
					if(body.remaining()>=bytes.remaining())
						body.put(bytes);
					else
						body.put(bytes.array(), 0, body.remaining());
					if(body.remaining()>0) //we need to read from the next ByteBuffer
						i++;
				}
				if(body.remaining()==0){ //We finished reading the request so we can create the new message
					body.flip();
					_messages.add(new HttpRequestMessage(operator, URI, protocol, headers, body));
					
					//remove all ByteBuffers we already read
					for (int j=0; j<i; j++) 
						_messageBuf.remove(0);
					if(bytes.remaining()==0) 
						_messageBuf.remove(0); //This is the end of the ByteBuffer - we can drop it.
				}else{
					// We couldn't finish reading the job so we need to return all ByteBuffer positions to 0 for next try
					for(int j=0; j<i; j++) 
						_messageBuf.get(j).position(0);
				}
				break;
			}
		}

	}

	@Override
	public synchronized boolean hasMessage() {
		proccessData();
		return !_messages.isEmpty();
	}

	@Override
	public synchronized HttpMessage nextMessage() {
		HttpMessage message = _messages.remove(0);
		return message;
	}

	@Override
	public ByteBuffer getBytesForMessage(HttpMessage msg)  throws CharacterCodingException {
		StringBuilder sb = new StringBuilder(msg.getBeginning());
		ByteBuffer bb = this._ascii_encoder.encode(CharBuffer.wrap(sb));
		ByteBuffer result=ByteBuffer.allocate(bb.remaining()+msg.body.remaining());
		result.put(bb);
		result.put(msg.body);
		result.flip();
		return result;
	}
}
