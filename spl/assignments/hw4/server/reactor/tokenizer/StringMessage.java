package tokenizer;

public class StringMessage implements Message<StringMessage> {
	private String message;
	
	public StringMessage(String message){
		this.message=message;
	}

    public void setMessage(String msg) {
        message = msg;
    }

	public String getMessage(){
		return message;
	}
	
	@Override
	public String toString() {
		return message;
	}
	
	@Override
	public boolean equals(Object other) {
		return message.equals(other);
	}
}
