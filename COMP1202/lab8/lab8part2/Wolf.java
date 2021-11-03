class Wolf extends Carnivore {
	public Wolf(String wolfName, Integer wolfAge) {
		super(wolfName, wolfAge);
	}

	public Wolf() {
		super();
	}

	@Override
	public void makeNoise() {
		System.out.println("wooof");
	}
}