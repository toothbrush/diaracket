package fr.diaspec.webcam.common.maybe;

public class Just<T> extends Maybe<T> {

	public T just_value;
	
	public Just (T v){
		this.just_value = v;	
	}
}