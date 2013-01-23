package tokenizer.http;

import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import tokenizer.Message;

public abstract class HttpMessage implements Message<HttpMessage> {
	public final String protocol;
	public final Map<String, List<String>> headers;
	public final ByteBuffer body;
	
	public HttpMessage(String protocol, Map<String, List<String>> headers, ByteBuffer body){
		this.protocol=protocol;
		this.headers=headers;
		this.body=body;
	}
	
	/**
	 * A static helper function for parsing a {@link String} of a header line into a headers {@link Map}.  
	 * @param headers A headers {@link Map}
	 * @param header A {@link String} of a header line
	 */
	public static void addHeader(Map<String, List<String>> headers, String header){
		String name=header.substring(0,header.indexOf(':')).toLowerCase();
		List<String> values=new LinkedList<String>();
		String tmp[]=header.substring(header.indexOf(':')+1).split(",");
		for(int i =0; i < tmp.length ; i++)
			values.add(tmp[i]);
		headers.put(name, values);
	}
	
	/**
	 * This function returns the beginning of the HTTP message (initial line, headers and the empty line) 
	 * @return The beginning of the HTTP message
	 */
	public abstract String getBeginning();
}
