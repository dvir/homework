package protocol;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import tokenizer.http.*;

/**
 * a simple implementation of the server protocol interface
 */
public class HttpProtocol implements AsyncServerProtocol<HttpMessage> {
	private final static String sample_xml;
	private final static String sample_html;
	private final static byte sample_img[]=new byte[63122];
	
	private boolean _shouldClose = false;
	private boolean _connectionTerminated = false;

	
	/**
	 *  This is a static constructor that initialize the static fields.
	 */
	static{
		sample_xml ="<?xml version=\"1.0\" encoding=\"UTF-8\"?><JobRequest>	<InputRepresentation id=\"3\"/>	<effectsList>		<cvtColor>			<code> CV_RGB2GRAY </code>		</cvtColor>		<resize>			<scaleFactorX> 0.5 </scaleFactorX>			<scaleFactorY> 0.5 </scaleFactorY>			<interpolation> INTER_LINEAR </interpolation>		</resize>		<GaussianBlur>			<kSize> 3 </kSize>			<sigmaX> 2 </sigmaX>			<sigmaY> 2 </sigmaY>			<borderType> BORDER_REPLICATE </borderType>		</GaussianBlur>	</effectsList></JobRequest>";
		sample_html="<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"><head><title>HTTP/1.1 404 Not Found</title></head><body>To get the sample picture click <a href=\"pic\">here - GET /pic HTTP/1.1</a><br/>To get the sample XML file click <a href=\"xml\">here - GET /xml HTTP/1.1</a><br/>To upload a picture use <b>curl -H \"content-type: image/jpg\" -T image1.jpg [server-address]/pic > image2.jpg</b> request<br/>To upload an XML file use <b>curl -H \"content-type: text/xml\" -T job1.xml [server-address]/xml >job2.xml</b> request<br/></body></html>";
		try {
			File file=new File("image1.jpg");
			FileInputStream fis = new FileInputStream(file);
				fis.read(sample_img); //We know that image.img is smaller than bug.length
			fis.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
	}
	
	@Override
	public HttpMessage processMessage(HttpMessage msg) {     
		HttpRequestMessage request=(HttpRequestMessage)msg;
		
		HttpResponseCode code=null;
		String protocol="HTTP/1.1";
		Map<String, List<String>> headers=new Hashtable<String, List<String>>();
		ByteBuffer body=null;
		
		if (this._connectionTerminated) {
			return null;
		}
		if (this.isEnd(msg)) {
			this._shouldClose = true;
		}
		
		switch (request.operator){
		case PUT:
			if(request.URI.equals("/pic")){ //The client uploaded a picture - we return a message with the picture in the body.
				code=HttpResponseCode.S201;
				headers.put("content-length", request.headers.get("content-length"));
				headers.put("content-type", request.headers.get("content-type"));
				body=ByteBuffer.allocate(request.body.remaining());
				body.put(request.body);
				body.flip();
			}else if(request.URI.equals("/xml")){ //The client uploaded an XML file - we return a message with the XML file in the body.
				code=HttpResponseCode.S201;
				headers.put("content-length", request.headers.get("content-length"));
				headers.put("content-type", request.headers.get("content-type"));
				body=ByteBuffer.allocate(request.body.remaining());
				body.put(request.body);
				body.flip();
			}else{
				code=HttpResponseCode.S404; //The default answer - an HTML page
				byte bytes[]=sample_html.getBytes(Charset.forName("UTF-8"));
				HttpMessage.addHeader(headers, "Content-length: " + bytes.length);
				HttpMessage.addHeader(headers, "Content-type: text/html; charset=utf-8");
				body=ByteBuffer.wrap(bytes);
			}
			break;
		case GET:
			if(request.URI.equals("/pic")){ // The default picture message
				code=HttpResponseCode.S200;
				HttpMessage.addHeader(headers, "Content-length: " + sample_img.length);
				HttpMessage.addHeader(headers, "Content-type: image/jpg");
				body=ByteBuffer.wrap(sample_img);
			}else if(request.URI.equals("/xml")){ // The default XML message
				code=HttpResponseCode.S200;
				byte bytes[]=sample_xml.getBytes(Charset.forName("UTF-8"));
				HttpMessage.addHeader(headers, "Content-length: " + bytes.length);
				HttpMessage.addHeader(headers, "Content-type: text/xml; charset=utf-8");
				body=ByteBuffer.wrap(bytes);
			}else{
				code=HttpResponseCode.S404; //The default answer - an HTML page
				byte bytes[]=sample_html.getBytes(Charset.forName("UTF-8"));
				HttpMessage.addHeader(headers, "Content-length: " + bytes.length);
				HttpMessage.addHeader(headers, "Content-type: text/html; charset=utf-8");
				body=ByteBuffer.wrap(bytes);
			}
			break;
		}
		
		return new HttpResponseMessage(code, protocol, headers, body);
	}

	/**
	 * In HTTP we can always close the connection after answering the client and this is what we do by returning true here.
	 */
	@Override
	public boolean isEnd(HttpMessage msg) {
		return true;
	}

	@Override
	public boolean shouldClose() {
		return this._shouldClose;
	}

	@Override
	public void connectionTerminated() {
		this._connectionTerminated = true;
	}
}
