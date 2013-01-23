package tokenizer.http;

import java.nio.ByteBuffer;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class HttpResponseMessage extends HttpMessage{
	public final HttpResponseCode responseCode;
	
	public HttpResponseMessage(HttpResponseCode responseCode, String protocol, Map<String, List<String>> headers, ByteBuffer body){
		super(protocol, headers, body);
		this.responseCode=responseCode;
	}
	
	@Override
	public String getBeginning(){
		StringBuilder sb=new StringBuilder(1028);
		sb.append(protocol);
		sb.append(" ");
		sb.append(responseCode.StatusCode());
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
