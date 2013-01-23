package tokenizer.http;

public enum HttpResponseCode {
	S200 (200, "OK"),
	S201 (201, "Created"),
	S202 (202, "Accepted"),
	S404 (404, "Not Found");
	
	private final int code; 
	private final String reason;
	
	private HttpResponseCode(int code, String reason){
		this.code=code;
		this.reason=reason;
	}
	
	public String StatusCode(){
		return code+" "+reason;
	}
}
