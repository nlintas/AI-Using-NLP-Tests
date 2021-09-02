package lab1;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

/**
 * This class implements a state of the "Cannibals and Missionaries" puzzle. As
 * crossing a river involves two banks, {@code missionaries} denotes the amount
 * of missionaries on the source bank, and
 * {@code totalMissionaries - missionaries} is the amount of missionaries on the
 * target bank. Same arithmetics applies to cannibals.
 * 
 * Initial author: Rodion Efremov the Finnish Open-Source Computer Scientist
 * Edited by: Nikolaos Lintas from University of Sheffield
 */
public class StateNode implements Iterable<StateNode> {

	/**
	 * The minimum allowed amount of cannibals or missionaries.
	 */
	private static final int MIN_TOTAL = 1;

	/**
	 * The minimum boat capacity.
	 */
	private static final int MIN_BOAT_CAPACITY = 1;

	/**
	 * This enumeration enumerates all possible boat locations.
	 */
	public enum BoatLocation {

		/**
		 * The boat location where all figures start.
		 */
		SOURCE_BANK,

		/**
		 * The boat location all figures want to reach.
		 */
		TARGET_BANK
	}

	/**
	 * The amount of missionaries at the source bank.
	 */
	private final int missionaries;

	/**
	 * The amount of cannibals at the source bank.
	 */
	private final int cannibals;

	/**
	 * The total amount of missionaries involved in the game.
	 */
	private final int totalMissionaries;

	/**
	 * The total amount of cannibals involved in the game.
	 */
	private final int totalCannibals;

	/**
	 * The amount of places in the boat.
	 */
	private final int boatCapacity;

	/**
	 * The location of the boat.
	 */
	private final BoatLocation boatLocation;

	/**
	 * Constructs this state.
	 * 
	 * @param missionaries      amount of missionaries at a bank.
	 * @param cannibals         amount of cannibals at the same ban.
	 * @param totalMissionaries total amount of missionaries.
	 * @param totalCannibals    total amount of cannibals.
	 * @param boatCapacity      total amount of places in the boat.
	 * @param boatLocation      the location of the boat.
	 */
	public StateNode(int missionaries, int cannibals, int totalMissionaries, int totalCannibals, int boatCapacity,
			BoatLocation boatLocation) {
		Objects.requireNonNull(boatLocation, "Boat location is null.");
		checkTotalMissionaries(totalMissionaries);
		checkTotalCannibals(totalCannibals);
		checkMissionaryCount(missionaries, totalMissionaries);
		checkCannibalCount(cannibals, totalCannibals);
		checkBoatCapacity(boatCapacity);

		this.missionaries = missionaries;
		this.cannibals = cannibals;
		this.totalMissionaries = totalMissionaries;
		this.totalCannibals = totalCannibals;
		this.boatCapacity = boatCapacity;
		this.boatLocation = boatLocation;
	}

	/**
	 * Creates the source state node.
	 * 
	 * @param totalMissionaries the total amount of missionaries.
	 * @param totalCannibals    the total amount of cannibals.
	 * @param boatCapacity      the total amount of places in the boat.
	 * @return the initial state node.
	 */
	public static StateNode getInitialStateNode(int totalMissionaries, int totalCannibals, int boatCapacity) {
		return new StateNode(totalMissionaries, totalCannibals, totalMissionaries, totalCannibals, boatCapacity,
				BoatLocation.SOURCE_BANK);
	}

	/**
	 * Checks whether this state encodes a solution state, in which all figures are
	 * safely at the target bank.
	 * 
	 * @return {@code true} if this state is a solution state.
	 */
	public boolean isSolutionState() {
		return boatLocation == BoatLocation.TARGET_BANK && missionaries == 0 && cannibals == 0;
	}

	/**
	 * Checks whether this state is terminal, which is the case whenever at some
	 * bank cannibals outnumber missionaries.
	 * 
	 * @return {@code true} if this state is terminal.
	 */
	public boolean isTerminalState() {
		if (missionaries > 0 && missionaries < cannibals) {
			// At the source bank, cannibals outnumber missionaries. Game over.
			return true;
		}

		int missionariesAtTargetBank = totalMissionaries - missionaries;
		int cannibalsAtTargetBank = totalCannibals - cannibals;

		if (missionariesAtTargetBank > 0 && missionariesAtTargetBank < cannibalsAtTargetBank) {
			// At the target bank, cannibals outnumber missionaries. Game over.
			return true;
		}

		return false;
	}

	/**
	 * Returns an iterator over this state's neighbor states.
	 * 
	 * @return an iterator.
	 */
	@Override
	public Iterator<StateNode> iterator() {
		return new NeighborStateIterator();
	}

	/**
	 * {@inheritDoc }
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();

		int missionaryFieldLength = ("" + totalMissionaries).length();
		int cannibalFieldLength = ("" + totalCannibals).length();

		// Situation at the source bank.
		sb.append(String.format("[m: %" + missionaryFieldLength + "d", missionaries));
		sb.append(String.format(", c: %" + cannibalFieldLength + "d]", cannibals));

		// Draw boat location.
		switch (boatLocation) {
		case SOURCE_BANK: {
			sb.append("v ~~~  ");
			break;
		}

		case TARGET_BANK: {
			sb.append("  ~~~ v");
			break;
		}
		}

		// Situation at the destination bank.
		sb.append(String.format("[m: %" + missionaryFieldLength + "d", totalMissionaries - missionaries));
		sb.append(String.format(", c: %" + cannibalFieldLength + "d]", totalCannibals - cannibals));
		return sb.toString();
	}

	/**
	 * {@inheritDoc }
	 */
	@Override
	public boolean equals(Object o) {
		if (!(o instanceof StateNode)) {
			return false;
		}

		StateNode other = (StateNode) o;
		return missionaries == other.missionaries && cannibals == other.cannibals
				&& totalMissionaries == other.totalMissionaries && totalCannibals == other.totalCannibals
				&& boatLocation == other.boatLocation;
	}

	/**
	 * {@inheritDoc }
	 */
	@Override
	public int hashCode() {
		// Generated by NetBeans.
		int hash = 7;
		hash = 31 * hash + this.missionaries;
		hash = 31 * hash + this.cannibals;
		hash = 31 * hash + this.totalMissionaries;
		hash = 31 * hash + this.totalCannibals;
		hash = 31 * hash + Objects.hashCode(this.boatLocation);
		return hash;
	}

	// Implements the actual iterator.
	private class NeighborStateIterator implements Iterator<StateNode> {

		private final Iterator<StateNode> iterator;

		public NeighborStateIterator() {
			this.iterator = generateNeighbors();
		}

		@Override
		public boolean hasNext() {
			return iterator.hasNext();
		}

		@Override
		public StateNode next() {
			return iterator.next();
		}

		// Populates the list of neighbor states.
		private Iterator<StateNode> generateNeighbors() {
			if (isTerminalState()) {
				// Ignore terminal state nodes.
				return Collections.<StateNode>emptyIterator();
			}

			List<StateNode> list = new ArrayList<>();

			switch (StateNode.this.boatLocation) {
			case SOURCE_BANK: {
				trySendFromSourceBank(list);
				break;
			}

			case TARGET_BANK: {
				trySendFromTargetBank(list);
				break;
			}
			}

			return list.iterator();
		}

		// Attempts to send some figures from the source bank to the target
		// bank.
		private void trySendFromSourceBank(List<StateNode> list) {
			int availableMissionaries = Math.min(missionaries, boatCapacity);
			int availableCannibals = Math.min(cannibals, boatCapacity);

			for (int capacity = 1; capacity <= boatCapacity; ++capacity) {
				for (int m = 0; m <= availableMissionaries; ++m) {
					for (int c = 0; c <= availableCannibals; ++c) {
						if (0 < c + m && c + m <= capacity) {
							list.add(new StateNode(missionaries - m, cannibals - c, totalMissionaries, totalCannibals,
									boatCapacity, BoatLocation.TARGET_BANK));
						}
					}
				}
			}
		}

		// Attempts to send some figures from the target bank to the source
		// bank.
		private void trySendFromTargetBank(List<StateNode> list) {
			int availableMissionaries = Math.min(totalMissionaries - missionaries, boatCapacity);
			int availableCannibals = Math.min(totalCannibals - cannibals, boatCapacity);

			for (int capacity = 1; capacity <= boatCapacity; ++capacity) {
				for (int m = 0; m <= availableMissionaries; ++m) {
					for (int c = 0; c <= availableCannibals; ++c) {
						if (0 < c + m && c + m <= capacity) {
							list.add(new StateNode(missionaries + m, cannibals + c, totalMissionaries, totalCannibals,
									boatCapacity, BoatLocation.SOURCE_BANK));
						}
					}
				}
			}
		}
	}

	/**
	 * Checks that the total amount of missionaries is sensible.
	 * 
	 * @param totalMissionaries total amount of missionaries.
	 */
	private static void checkTotalMissionaries(int totalMissionaries) {
		checkIntNotLess(totalMissionaries, MIN_TOTAL, "The total amount of missionaries is too small: "
				+ totalMissionaries + ". Should be at least " + MIN_TOTAL);
	}

	/**
	 * Checks that the total amount of cannibals is sensible.
	 * 
	 * @param totalCannibals total amount of cannibals.
	 */
	private static void checkTotalCannibals(int totalCannibals) {
		checkIntNotLess(totalCannibals, MIN_TOTAL,
				"The total amount of cannibals is too small: " + totalCannibals + ". Should be at least " + MIN_TOTAL);
	}

	/**
	 * Checks that missionary count is in order.
	 * 
	 * @param missionaries      the amount of missionaries at the source bank.
	 * @param totalMissionaries total amount of missionaries in the game.
	 */
	private static void checkMissionaryCount(int missionaries, int totalMissionaries) {
		checkNotNegative(missionaries, "Negative amount of missionaries: " + missionaries);
		checkIntNotLess(totalMissionaries, missionaries, "Missionaries at a bank (" + missionaries + "), "
				+ "missionaries in total (" + totalMissionaries + ").");
	}

	/**
	 * Checks that cannibal count is in order.
	 * 
	 * @param cannibals      the amount of cannibals at the source bank.
	 * @param totalCannibals total amount of cannibals in the game.
	 */
	private static void checkCannibalCount(int cannibals, int totalCannibals) {
		checkNotNegative(cannibals, "Negative amount of cannibals: " + cannibals);
		checkIntNotLess(totalCannibals, cannibals,
				"Cannibals at a bank (" + cannibals + "), " + "cannibals in total (" + totalCannibals + ").");
	}

	/**
	 * Checks that boat capacity is sensible.
	 * 
	 * @param boatCapacity the boat capacity.
	 */
	private static void checkBoatCapacity(int boatCapacity) {
		checkIntNotLess(boatCapacity, MIN_BOAT_CAPACITY,
				"Boat capacity too small: " + boatCapacity + ", " + "must be at least " + MIN_BOAT_CAPACITY + ".");
	}

	/**
	 * Checks that {@code integer} is no less than {@code minimum}, and if it is,
	 * throws an exception with message {@code errorMessage}.
	 * 
	 * @param integer      the integer to check.
	 * @param minimum      the minimum allowed value of {@code integer}.
	 * @param errorMessage the error message.
	 * @throws IllegalArgumentException if {@code integer < minimum}.
	 */
	private static void checkIntNotLess(int integer, int minimum, String errorMessage) {
		if (integer < minimum) {
			throw new IllegalArgumentException(errorMessage);
		}
	}

	/**
	 * Checks that {@code integer} is not negative.
	 * 
	 * @param integer      the integer to check.
	 * @param errorMessage the error message for the exception upon failure.
	 */
	private static void checkNotNegative(int integer, String errorMessage) {
		checkIntNotLess(integer, 0, errorMessage);
	}
}