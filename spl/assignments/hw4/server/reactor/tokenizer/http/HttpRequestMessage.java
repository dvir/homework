package tokenizer.http;

import java.nio.ByteBuffer;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class HttpRequestMessage extends HttpMessage{
	/**
	 * GET, PUT, POST, etc.
	 */
	public final HttpOperator operator;
	public final String URI;
	
	public HttpRequestMessage(HttpOperator operator, String URI, String protocol, Map<String, List<String>> headers, ByteBuffer body){
		super(protocol, headers, body);
		this.operator=operator;
		this.URI=URI;
	}
	
	@Override
	public String getBeginning(){
		StringBuilder sb=new StringBuilder(1028);
		sb.append(operator);
		sb.append(" ");
		sb.append(URI);
		sb.append(" ");
		sb.append(protocol);
		sb.append("\r\n");
		for (Entry<String, List<String>> header : headers.entrySet()) {
			sb.append(header.getKey());
			sb.append(": ");
			for (String value : header.getValue()) {
				sb.append(value);
				sb.append(", ");
			}
			sb.delete(sb.lastIndexOf(","), sb.length());
			sb.append("\r\n");
		}
		sb.append("\r\n");
		return sb.toString();
	}
}
